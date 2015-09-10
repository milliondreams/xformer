package com.tuplejump.xformer

import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{JsArray, JsString, JsValue}

import scala.xml.XML

class XmlToJsonSpec extends WordSpec with Matchers {

  "XmlJsonHelper" when {
    "transforming xml to json" should {
      "support simple array" in {
        val input = XML.loadString( """<dependencies>
                                      |    <dependency>
                                      |      <groupId>group-c</groupId>
                                      |      <artifactId>artifact-b</artifactId>
                                      |      <version>1.0</version>
                                      |      <type>war</type>
                                      |      <scope>runtime</scope>
                                      |    </dependency>
                                      |    <dependency>
                                      |      <groupId>group-a</groupId>
                                      |      <artifactId>artifact-b</artifactId>
                                      |      <version>1.0</version>
                                      |      <type>bar</type>
                                      |      <scope>runtime</scope>
                                      |    </dependency>
                                      |  </dependencies>""".stripMargin)

        val output = XmlJsonHelper.xmlToJson(input)
        println(output)
        val actualResult: Seq[JsValue] = ((output \\ "dependency")(0)).as[JsArray].value
        actualResult.size should be(2)
      }

      "support a polluted array" in {
        val input = XML.loadString( """<dependencies>
                                      |<total>3</total>
                                      |<dependency>
                                      |<groupId>group-c</groupId>
                                      |<artifactId>artifact-b</artifactId>
                                      |<version>1.0</version>
                                      |<type>war</type>
                                      |<scope>runtime</scope>
                                      |</dependency>
                                      |<dependency>
                                      |<groupId>group-a</groupId>
                                      |<artifactId>artifact-b</artifactId>
                                      |<version>1.0</version>
                                      |<type>bar</type>
                                      |<scope>runtime</scope>
                                      |</dependency>
                                      |<region>A</region>
                                      |</dependencies>""".stripMargin)

        val output = XmlJsonHelper.xmlToJson(input)
        println(output)
        val dependenciesNode: JsValue = ((output \\ "dependencies")(0))
        val totalValue: String = (dependenciesNode \ "total").as[JsString].value
        totalValue.toInt should be (3)
        val dependencyNodes: Seq[JsValue] = (dependenciesNode \ "dependency").as[JsArray].value
        dependencyNodes.size should be(2)
      }
    }
  }

}
