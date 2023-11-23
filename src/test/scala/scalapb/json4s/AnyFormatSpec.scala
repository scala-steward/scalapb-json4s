package scalapb.json4s

import com.google.protobuf.any.{Any => PBAny}
import com.google.protobuf.struct.{Value, Struct}
import jsontest.anytests.{AnyTest, ManyAnyTest, AnyContainer}
import org.json4s.jackson.JsonMethods._

import scala.language.existentials
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class AnyFormatSpec extends AnyFlatSpec with Matchers with JavaAssertions {
  val RawExample = AnyTest("test")

  val RawJson = parse(s"""{"field":"test"}""")

  val AnyExample = PBAny.pack(RawExample)

  val AnyWithValue = PBAny.pack(Value())

  val AnyJson = parse(
    s"""{"@type":"type.googleapis.com/jsontest.AnyTest","field":"test"}"""
  )

  val CustomPrefixAny = PBAny.pack(RawExample, "example.com/")

  val CustomPrefixJson = parse(
    s"""{"@type":"example.com/jsontest.AnyTest","field":"test"}"""
  )

  val ManyExample = ManyAnyTest(
    Seq(
      PBAny.pack(AnyTest("1")),
      PBAny.pack(AnyTest("2"))
    )
  )

  val ManyPackedJson = parse(
    """
      |{
      |  "@type": "type.googleapis.com/jsontest.ManyAnyTest",
      |  "fields": [
      |    {"@type": "type.googleapis.com/jsontest.AnyTest", "field": "1"},
      |    {"@type": "type.googleapis.com/jsontest.AnyTest", "field": "2"}
      |  ]
      |}
    """.stripMargin
  )

  override def registeredCompanions = Seq(AnyTest, ManyAnyTest)

  // For clarity
  def UnregisteredPrinter = JsonFormat.printer

  def UnregisteredParser = JsonFormat.parser

  "Any" should "fail to serialize if its respective companion is not registered" in {
    an[IllegalStateException] must be thrownBy UnregisteredPrinter.toJson(
      AnyExample
    )
  }

  "Any" should "fail to deserialize if its respective companion is not registered" in {
    a[JsonFormatException] must be thrownBy UnregisteredParser.fromJson[PBAny](
      AnyJson
    )
  }

  "Any" should "serialize correctly if its respective companion is registered" in {
    ScalaJsonPrinter.toJson(AnyExample) must be(AnyJson)
  }

  "Any" should "fail to serialize with a custom URL prefix if specified" in {
    an[IllegalStateException] must be thrownBy ScalaJsonPrinter.toJson(
      CustomPrefixAny
    )
  }

  "Any" should "fail to deserialize for a non-Google-prefixed type URL" in {
    a[JsonFormatException] must be thrownBy ScalaJsonParser.fromJson[PBAny](
      CustomPrefixJson
    )
  }

  "Any" should "deserialize correctly if its respective companion is registered" in {
    ScalaJsonParser.fromJson[PBAny](AnyJson) must be(AnyExample)
  }

  "Any" should "be serialized the same as in Java (and parsed back to original)" in {
    assertJsonIsSameAsJava(AnyExample)
  }

  "Any" should "resolve printers recursively" in {
    val packed = PBAny.pack(ManyExample)
    ScalaJsonPrinter.toJson(packed) must be(ManyPackedJson)
  }

  "Any" should "resolve parsers recursively" in {
    ScalaJsonParser.fromJson[PBAny](ManyPackedJson).unpack[ManyAnyTest] must be(
      ManyExample
    )
  }

  "Any" should "serialize a struct value" in {
    val optionalAnyJson = parse("""{
      "optionalAny": {
        "@type": "type.googleapis.com/google.protobuf.Value",
        "value": {"foo": 1.0}
      }
    }""")

    val input = ScalaJsonParser.fromJson[AnyContainer](optionalAnyJson)

    input.getOptionalAny.unpack[com.google.protobuf.struct.Value] must be(
      Value().withStructValue(
        Struct(fields = Map("foo" -> Value().withNumberValue(1)))
      )
    )

    ScalaJsonPrinter.toJson(input) must be(optionalAnyJson)
  }

  "Any" should "serialize a timestamp value" in {
    val optionalAnyJson = parse("""{
      "optionalAny": {
        "@type": "type.googleapis.com/google.protobuf.Timestamp",
        "value": "1970-01-01T00:00:00Z"
      }
    }""")

    val input = ScalaJsonParser.fromJson[AnyContainer](optionalAnyJson)

    input.getOptionalAny
      .unpack[com.google.protobuf.timestamp.Timestamp] must be(
      com.google.protobuf.timestamp.Timestamp()
    )

    ScalaJsonPrinter.toJson(input) must be(optionalAnyJson)
  }

  "Any" should "work when nested" in {
    val nestedAny = parse("""{
        |   "optionalAny": {
        |     "@type": "type.googleapis.com/google.protobuf.Any",
        |     "value": {
        |       "@type": "type.googleapis.com/jsontest.AnyTest",
        |       "field": "Boo"
        |     }
        |   }
        |}""".stripMargin)

    val input = ScalaJsonParser.fromJson[AnyContainer](nestedAny)
    ScalaJsonPrinter.toJson(input) must be(nestedAny)
  }
}
