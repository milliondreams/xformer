package com.tuplejump.xformer

import play.api.libs.json._

import scala.xml._
import scala.xml.Utility._

object XmlJsonHelper {

  def jsonToXml(jsonData: JsValue) = trim(internalJsonToXml(jsonData)).child.head

  private def buildNode(key: String, result: Elem, cn: Seq[Node]): Node = {
    val finalResult = result.copy(null, key, Null, TopScope, false, cn)
    val finalValue = trim(finalResult)
    finalValue
  }


  private def internalJsonToXml(jsonData: JsValue): Node = {
    val xmlResult = jsonData match {
      case JsObject(fields) => {
        fields.map {
          case (key, value) => {
            val result = Elem(null, key, Null, TopScope, false)
            val childNodes = internalJsonToXml(value)
            trim(childNodes) match {
              case <result>{x}</result> => //removing additional result tag added in recursion
                buildNode(key, result, x)
              case other if other.child.size > 1 =>
                if (other.child.head.label == "result") { // when there is an array of result elememnts
                  other.child.map {
                    cn =>
                      buildNode(key, result, cn.child)
                  }
                } else {
                  buildNode(key, result, other.child) // when there are no additional result tags
                }
            }
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
    def leaf_?(node: Node) = !node.descendant.exists(_.isInstanceOf[Elem])
    def array_?(nodeNames: Seq[String]) = nodeNames.size != 1 && nodeNames.distinct.size == 1
    def pollutedArray_?(nodeNames: Seq[String]) = nodeNames.size != 1 && nodeNames.size > nodeNames.distinct.size
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
      case XValue(s) => {
       /* val numberPattern: String = "[+-]?\\d+(\\.\\d+)?"
        val isNumeric: Boolean = s.matches(numberPattern)
        println(s+" "+isNumeric)
        if(isNumeric) {
          if(s.contains(".")) Json.toJson(s.toDouble) else Json.toJson(s.toLong)
        } else {
          Json.toJson(s)
        }*/
        Json.toJson(s)
      }
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

    def generateArray(nodes: NodeSeq, label: String): List[XElem] = {
      val arr = XArray(nodes.toList.flatMap {
        n =>
          if (leaf_?(n) && n.attributes.length == 0) XValue(n.text) :: Nil
          else buildNodes(n)
      })
      XLeaf((label, arr), Nil) :: Nil
    }

    def buildNodes(xml: NodeSeq): List[XElem] = xml match {
      case n: Node =>
        if (empty_?(n)) XLeaf((nameOf(n), XValue("")), buildAttributes(n)) :: Nil
        else if (leaf_?(n)) {
          XLeaf((nameOf(n), XValue(n.text)), buildAttributes(n)) :: Nil
        }
        else {
          val children = directChildren(n)
          val childNodes: List[(String, XElem)] = children.groupBy(_.label).map {
            case (label, elems) =>
              (label, buildNodes(elems).head)
          }.toList
          XNode(buildAttributes(n) ++ childNodes) :: Nil
        }
      case nodes: NodeSeq =>
        val allLabels = nodes.map(_.label)
        if (array_?(allLabels)) {
          generateArray(nodes, allLabels.head)
        } else nodes.toList.flatMap(buildNodes)
    }

    val generatedNodes: List[XElem] = buildNodes(xmlData)
    generatedNodes match {
      case List(x@XLeaf(_, _ :: _)) => toJValue(x)
      case List(x) => Json.obj(nameOf(xmlData.head) -> toJValue(x))
      case x => Json.toJson(x.map(toJValue))
    }
  }
}
