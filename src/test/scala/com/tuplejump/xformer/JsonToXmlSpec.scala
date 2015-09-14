package com.tuplejump.xformer

import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json.Json

import scala.xml.XML
import scala.xml.Utility._

class JsonToXmlSpec extends WordSpec with Matchers {

  "XmlJsonHelper" when {
    "transforming json to xml" should {
      "support simple array" in {
        val input = Json.parse(
          """{"dependencies":{
            |"dependency":[
            |{"groupId":"group-c","artifactId":"artifact-b","version":"1.0","type":"war","scope":"runtime"},
            |{"groupId":"group-a","artifactId":"artifact-b","version":"1.0","type":"bar","scope":"runtime"}
            |]}}""".stripMargin)

        println(input)

        val expectedOutput = XML.loadString( """<dependencies>
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

        val output = XmlJsonHelper.jsonToXml(input)
        println(output)
        println(trim(expectedOutput))
        output should be (trim(expectedOutput))
      }

      "support polluted array" in {
        val input = Json.parse(
          """{"dependencies":{
            |"total": "3",
            |"region":"A",
            |"dependency":[
            |{"groupId":"group-c","artifactId":"artifact-b","version":"1.0","type":"war","scope":"runtime"},
            |{"groupId":"group-a","artifactId":"artifact-b","version":"1.0","type":"bar","scope":"runtime"}
            |]}}""".stripMargin)

        println(input)

        val expectedOutput = XML.loadString( """<dependencies>
                                               |<total>3</total>
                                               |<region>A</region>
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

        val output = XmlJsonHelper.jsonToXml(input)
        println(output)
        println(trim(expectedOutput))
        output should be (trim(expectedOutput))
      }

      "support messy polluted array" in {
        val input = Json.parse(
          """{"dependencies":{
            |"total": "3",
            |"dependency":[
            |{"groupId":"group-c","artifactId":"artifact-b","version":"1.0","type":"war","scope":"runtime"},
            |{"groupId":"group-a","artifactId":"artifact-b","version":"1.0","type":"bar","scope":"runtime"}
            |],
            |"region":"A"}}""".stripMargin)

        println(input)

        val expectedOutput = XML.loadString( """<dependencies>
                                               |<total>3</total>
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
                                               |    <region>A</region>
                                               |  </dependencies>""".stripMargin)

        val output = XmlJsonHelper.jsonToXml(input)
        println(output)
        println(trim(expectedOutput))
        output should be (trim(expectedOutput))
      }
    }
  }
}
