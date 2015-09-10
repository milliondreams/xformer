package com.tuplejump.xformer

import play.api.http.MimeTypes
import play.api.libs.iteratee.{Enumeratee, Iteratee}
import play.api.libs.json.{JsValue, Json}
import play.api.mvc._
import play.mvc.Http
import play.api.libs.concurrent.Execution.Implicits._

import scala.xml.{XML, Node}

object JsonFilter extends EssentialFilter {
  override def apply(next: EssentialAction): EssentialAction = new EssentialAction {
    override def apply(request: RequestHeader): Iteratee[Array[Byte], Result] = {
      val resp = request.contentType match {
        case Some(MimeTypes.JSON) =>
          val newHeaderPairs: Seq[(String, Seq[String])] = (request.headers.toMap + (Http.HeaderNames.CONTENT_TYPE -> Seq(MimeTypes.XML))).toSeq

          val newHeaders = new Headers {
            override protected val data: Seq[(String, Seq[String])] = newHeaderPairs
          }

          XformerEnumeratees.json2Xml &>> next(request.copy(headers = newHeaders))

        case _ | None =>
          next(request)
      }

      if (request.accepts(MimeTypes.XML)) {
        resp
      } else if (request.accepts(MimeTypes.JSON)) {
        resp.map {
          result =>
            val jsonBody = result.body &> XformerEnumeratees.xml2Json

            result.copy(body = jsonBody).withHeaders(Http.HeaderNames.CONTENT_TYPE -> MimeTypes.JSON)
        }
      } else {
        resp
      }
    }
  }
}

object XformerEnumeratees {
  val json2Xml: Enumeratee[Array[Byte], Array[Byte]] = {
    val arrayConcat: Enumeratee[Array[Byte], Array[Byte]] = Enumeratee.grouped(Iteratee.consume[Array[Byte]]())
    val jsonParser: Enumeratee[Array[Byte], JsValue] = Enumeratee.map[Array[Byte]](Json.parse)
    val convertAndSerialize: Enumeratee[JsValue, Array[Byte]] = Enumeratee.map[JsValue](j => XmlJsonHelper.jsonToXml(j).toString().getBytes())
    arrayConcat ><> jsonParser ><> convertAndSerialize
  }

  val xml2Json: Enumeratee[Array[Byte], Array[Byte]] = {
    val arrayConcat: Enumeratee[Array[Byte], Array[Byte]] = Enumeratee.grouped(Iteratee.consume[Array[Byte]]())
    val xmlParser: Enumeratee[Array[Byte], Node] = Enumeratee.map[Array[Byte]](a => XML.loadString(new String(a)))
    val convertAndSerialize: Enumeratee[Node, Array[Byte]] = Enumeratee.map[Node](x => XmlJsonHelper.xmlToJson(x).toString().getBytes())
    arrayConcat ><> xmlParser ><> convertAndSerialize
  }
}
