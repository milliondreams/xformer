package com.intellivision.xformer

import play.api.libs.json._

import scala.xml._

object XmlJsonHelper {

  def jsonToXml(jsonData: JsValue) = internalJsonToXml(jsonData).child.head

  private def internalJsonToXml(jsonData: JsValue): Node = {
    val xmlResult = jsonData match {
      case JsObject(fields) => {
        fields.map {
          case (key, value) => {
            val result = Elem(null, key, Null, TopScope, false)
            result.copy(null, key, Null, TopScope, false, internalJsonToXml(value).child)
          }
        }
      }
      case JsString(content) => Text(content)
      case JsBoolean(bool) => Text(bool.toString)
      case JsNumber(num) => Text(num.toString())
      case JsArray(jsonArray) => jsonArray flatMap {
        s => internalJsonToXml(s)
      }
      case JsNull => Text("null")
      case j@JsUndefined() => <error>{Text(j.toString())}</error>
    }
    <result>{xmlResult}</result>
  }

  def xmlToJson(xmlData: NodeSeq): JsValue = {
    sealed trait XElem
    case class XValue(value: String) extends XElem
    case class XLeaf(value: (String, XElem), attributes: List[(String, XValue)]) extends XElem
    case class XNode(fields: List[(String, XElem)]) extends XElem
    case class XArray(elems: List[XElem]) extends XElem

    def empty_?(node: Node) = node.child.isEmpty
    def leaf_?(node: Node) = !node.descendant.find(_.isInstanceOf[Elem]).isDefined
    def array_?(nodeNames: Seq[String]) = nodeNames.size != 1 && nodeNames.toList.distinct.size == 1
    def directChildren(n: Node): NodeSeq = n.child.filter(c => c.isInstanceOf[Elem])
    def nameOf(n: Node) = (if (Option(n.prefix).nonEmpty) n.prefix + ":" else "") + n.label
    def buildAttributes(n: Node) = n.attributes.map((a: MetaData) => (a.key, XValue(a.value.text))).toList


    def mkFields(xs: List[(String, XElem)]) =
      xs.flatMap {
        case (name, value) => (value, toJValue(value)) match {
          case (XLeaf(v, x :: xs), o: JsObject) => o :: Nil
          case (_, json) => Json.obj(name -> json) :: Nil
        }
      }


    def toJValue(x: XElem): JsValue = x match {
      case XValue(s) => Json.toJson(s)
      case XLeaf((name, value), attributes) => (value, attributes) match {
        case (_, Nil) => toJValue(value)
        case (XValue(""), xs) => Json.toJson(mkFields(xs))
        case (_, xs) => Json.obj(Json.obj(name -> toJValue(value)).toString() -> Json.toJson(mkFields(xs)))
      }
      case XNode(xs) => {
        val result = mkFields(xs).reduce(_ ++ _)
        Json.toJson(result)
      }
      case XArray(elems) => Json.toJson(elems.map(toJValue))
    }

    def buildNodes(xml: NodeSeq): List[XElem] = xml match {
      case n: Node =>
        if (empty_?(n)) XLeaf((nameOf(n), XValue("")), buildAttributes(n)) :: Nil
        else if (leaf_?(n)) XLeaf((nameOf(n), XValue(n.text)), buildAttributes(n)) :: Nil
        else {
          val children = directChildren(n)
          XNode(buildAttributes(n) ++ children.map(nameOf).toList.zip(buildNodes(children))) :: Nil
        }
      case nodes: NodeSeq =>
        val allLabels = nodes.map(_.label)
        if (array_?(allLabels)) {
          val arr = XArray(nodes.toList.flatMap {
            n =>
              if (leaf_?(n) && n.attributes.length == 0) XValue(n.text) :: Nil
              else buildNodes(n)
          })
          XLeaf((allLabels(0), arr), Nil) :: Nil
        } else nodes.toList.flatMap(buildNodes)
    }

    buildNodes(xmlData) match {
      case List(x@XLeaf(_, _ :: _)) => toJValue(x)
      case List(x) => Json.obj(nameOf(xmlData.head) -> toJValue(x))
      case x => Json.toJson(x.map(toJValue))
    }
  }
}
