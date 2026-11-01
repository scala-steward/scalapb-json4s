package scalapb.json4s

import jsontest.extensions.extensions.{Extensible, ExtensionsProto}
import org.json4s.jackson.JsonMethods.parse
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class ExtensionFormatSpec extends AnyFlatSpec with Matchers {
  private val types = TypeRegistry.default.addFile(ExtensionsProto)
  private val parser = new Parser().withTypeRegistry(types)
  private val printer = new Printer().withTypeRegistry(types)

  "ProtoJSON extensions" should "preserve explicitly present defaults and repeated values" in {
    val json =
      """{"[jsontest.extensions.count]":0,"[jsontest.extensions.note]":"hi","[jsontest.extensions.numbers]":["1","-2"]}"""
    val message = parser.fromJsonString[Extensible](json)
    message.extension(ExtensionsProto.count) must be(Some(0))
    message.extension(ExtensionsProto.note) must be(Some("hi"))
    message.extension(ExtensionsProto.numbers) must be(Seq(1L, -2L))
    printer.toJson(message) must be(parse(json))
    Extensible.parseFrom(message.toByteArray) must be(message)
  }

  it should "preserve extensions inside nested messages, repeated messages, and maps" in {
    val json =
      """{"child":{"[jsontest.extensions.count]":1},"children":[{"[jsontest.extensions.count]":2}],"items":{"x":{"[jsontest.extensions.count]":3}}}"""
    val message = parser.fromJsonString[Extensible](json)
    message.getChild.extension(ExtensionsProto.count) must be(Some(1))
    message.children.head.extension(ExtensionsProto.count) must be(Some(2))
    message.items("x").extension(ExtensionsProto.count) must be(Some(3))
    printer.toJson(message) must be(parse(json))
  }

  it should "use registered companions for message extensions" in {
    val json = """{"[jsontest.extensions.details]":{"text":"hi"}}"""
    val message = parser.fromJsonString[Extensible](json)
    message.extension(ExtensionsProto.details).get.text must be(Some("hi"))
    printer.toJson(message) must be(parse(json))
  }

  it should "reject unknown names and invalid values while treating null as absent" in {
    for (
      json <- Seq(
        """{"[unknown]":1}""",
        """{"[jsontest.extensions.count]":"oops"}""",
        """{"[jsontest.extensions.numbers]":1}"""
      )
    ) {
      intercept[JsonFormatException](parser.fromJsonString[Extensible](json))
    }
    parser.fromJsonString[Extensible](
      """{"[jsontest.extensions.count]":null}"""
    ) must be(Extensible())
    parser.ignoringUnknownFields.fromJsonString[Extensible](
      """{"[unknown]":1}"""
    ) must be(Extensible())
  }
}
