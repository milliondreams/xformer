package com.tuplejump.xformer

import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.{Json, JsArray, JsString, JsValue}

import scala.xml.XML

class XmlToJsonSpec extends WordSpec with Matchers {

  "XmlJsonHelper" when {
    "transforming xml to json" should {

      "support single tag" in {
        val input = XML.loadString( """<foo>bar</foo>""".stripMargin)

        val output = XmlJsonHelper.xmlToJson(input)
        println(output)
        val actualResult: String = (output \ "foo").as[String]
        actualResult should be("bar")
      }

      "support multiple tags" in {
        val input = XML.loadString( """<data><foo>a</foo><bar>b</bar></data>""".stripMargin)

        val expectedOutput = Json.parse("""{"data":{"foo":"a","bar":"b"}}""")
        val output = XmlJsonHelper.xmlToJson(input)
        println(output)
        output should be(expectedOutput)
      }

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

        val expectedOutput = Json.parse(
          """{"dependencies":{
            |"dependency":[
            |{"groupId":"group-c","artifactId":"artifact-b","version":"1.0","type":"war","scope":"runtime"},
            |{"groupId":"group-a","artifactId":"artifact-b","version":"1.0","type":"bar","scope":"runtime"}
            |]}}""".stripMargin)

        val output = XmlJsonHelper.xmlToJson(input)
        println(output)
        output should be(expectedOutput)
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

        val expectedOutput = Json.parse(
          """{"dependencies":{
            |"total": "3",
            |"region":"A",
            |"dependency":[
            |{"groupId":"group-c","artifactId":"artifact-b","version":"1.0","type":"war","scope":"runtime"},
            |{"groupId":"group-a","artifactId":"artifact-b","version":"1.0","type":"bar","scope":"runtime"}
            |]}}""".stripMargin)

        val output = XmlJsonHelper.xmlToJson(input)
        println(output)
        output should be(expectedOutput)
      }
    }
  }

}
