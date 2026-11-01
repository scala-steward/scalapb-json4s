package scalapb.json4s

import com.google.protobuf.struct.Struct
import com.google.protobuf.wrappers.StringValue
import java.nio.charset.StandardCharsets
import java.util.Random
import jsontest.test.MyTest
import org.json4s.JsonAST._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class UnicodeSurrogateSpec extends AnyFlatSpec with Matchers {
  private val parser = new Parser()

  // Escape code units directly: an encoder/JSON printer could replace malformed input.
  private def escaped(value: String): String =
    value.map(c => "\\" + "u" + f"${c.toInt}%04x").mkString

  private def quoted(value: String): String = "\"" + value + "\""

  private val malformed = Seq(
    "\ud800",
    "\udbff",
    "\udc00",
    "\udfff", // Both ends of each surrogate range.
    "\ud800\ud800",
    "\udbff\udbff", // High followed by high.
    "\udc00\udc00",
    "\udfff\udfff", // Low followed by low.
    "\udc00\ud800",
    "\ude01\ud83d", // Reversed pairs.
    "\ud800x\udc00", // A separator cannot be skipped to complete a pair.
    "\ud800\udc00\ud800",
    "\ud800\udc00\udc00", // Invalid after a valid pair.
    "\ud800\ud800\udc00",
    "\udc00\ud800\udc00" // Invalid before a valid pair.
  )

  "JSON strings" should "reject malformed pairs at the beginning, middle, and end" in {
    for {
      invalid <- malformed
      (prefix, suffix) <- Seq(
        ("", ""),
        ("prefix", ""),
        ("", "suffix"),
        ("prefix", "suffix")
      )
    } {
      val value = prefix + invalid + suffix
      withClue(escaped(value) + ": ") {
        intercept[JsonFormatException](
          parser.fromJson[StringValue](JString(value))
        )
        // Cover decoded JSON escapes as well as literal UTF-16 supplied by the caller.
        for (
          text <- Seq(
            escaped(value),
            escaped(value).toUpperCase(java.util.Locale.ROOT).replace("U", "u"),
            value
          )
        ) {
          intercept[JsonFormatException](
            parser.fromJsonString[StringValue](quoted(text))
          )
          intercept[JsonFormatException](
            parser.fromJsonString[Struct]("{" + quoted(text) + ":\"x\"}")
          )
        }
        intercept[JsonFormatException](
          parser.fromJson[Struct](JObject(value -> JString("x")))
        )
      }
    }
  }

  it should "validate nested, repeated, map, and ignored fields in text and trees" in {
    for (value <- malformed) {
      val string = JString(value)
      val text = quoted(escaped(value))
      val inputs = Seq(
        (JObject("hello" -> string), "{\"hello\":" + text + "}"),
        (
          JObject("optMessage" -> JObject("hello" -> string)),
          "{\"optMessage\":{\"hello\":" + text + "}}"
        ),
        (
          JObject("primitiveSequence" -> JArray(List(JString("ok"), string))),
          "{\"primitiveSequence\":[\"ok\"," + text + "]}"
        ),
        (
          JObject("stringToInt32" -> JObject(value -> JInt(1))),
          "{\"stringToInt32\":{" + text + ":1}}"
        ),
        (
          JObject("ignored" -> JArray(List(JObject("nested" -> string)))),
          "{\"ignored\":[{\"nested\":" + text + "}]}"
        ),
        (JObject(value -> JInt(1)), "{" + text + ":1}")
      )
      for ((tree, json) <- inputs) {
        intercept[JsonFormatException](
          parser.ignoringUnknownFields.fromJson[MyTest](tree)
        )
        intercept[JsonFormatException](
          parser.ignoringUnknownFields.fromJsonString[MyTest](json)
        )
      }
    }
  }

  private def checkAccepted(value: String): Unit = {
    parser.fromJson[StringValue](JString(value)) must be(StringValue(value))
    parser.fromJsonString[StringValue](quoted(escaped(value))) must be(
      StringValue(value)
    )
    parser
      .fromJson[Struct](JObject(value -> JString(value)))
      .fields(value)
      .getStringValue must be(value)
    parser
      .fromJsonString[Struct](
        "{" + quoted(escaped(value)) + ":" + quoted(escaped(value)) + "}"
      )
      .fields(value)
      .getStringValue must be(value)
    // Java's JSON parser is an oracle for valid strings, but accepts lone surrogates too.
    val javaValue = com.google.protobuf.StringValue.newBuilder()
    com.google.protobuf.util.JsonFormat
      .parser()
      .merge(quoted(escaped(value)), javaValue)
    javaValue.getValue must be(value)
    parser.fromJsonString[StringValue](
      JsonFormat.toJsonString(StringValue(value))
    ) must be(StringValue(value))
  }

  it should "preserve Unicode boundaries, noncharacters, controls, and supplementary pairs" in {
    val points = Seq(0, 0x7f, 0x80, 0x7ff, 0x800, 0xd7ff, 0xe000, 0xfffd,
      0xfffe, 0xffff, 0x10000, 0x103ff, 0x10fc00, 0x10ffff, 0x1f601)
    val values = points.map(cp => new String(Character.toChars(cp)))
    (Seq("") ++ values ++ Seq(values.mkString, "a" + values.mkString + "z"))
      .foreach(checkAccepted)
    // In particular, actual supplementary characters are accepted without JSON escapes.
    val pair = new String(Character.toChars(0x1f601))
    parser.fromJsonString[StringValue](quoted(pair)) must be(StringValue(pair))
  }

  it should "distinguish escaped backslashes from Unicode escapes" in {
    Seq("\\ud800", "\\udc00", "\\uD800\\uDC00", "\\\\udfff").foreach(
      checkAccepted
    )
    parser.fromJsonString[StringValue]("\"\\\\ud800\"") must be(
      StringValue("\\ud800")
    )
    // A valid pair may mix an escaped code unit and a literal code unit.
    parser.fromJsonString[StringValue](
      quoted("\\" + "ud800" + "\udc00")
    ) must be(StringValue("\ud800\udc00"))
    parser.fromJsonString[StringValue](
      quoted("\ud800" + "\\" + "udc00")
    ) must be(StringValue("\ud800\udc00"))
  }

  it should "reject malformed Unicode escape syntax" in {
    for (suffix <- Seq("u", "u0", "u00", "u000", "uGGGG", "u12x4")) {
      intercept[JsonFormatException](
        parser.fromJsonString[StringValue](quoted("\\" + suffix))
      )
    }
  }

  private def checkAgainstJdk(value: String): Unit = {
    val expected = StandardCharsets.UTF_8.newEncoder().canEncode(value)
    val accepted = try {
      ParserLimits.checkValue(JString(value), 100)
      true
    } catch { case _: JsonFormatException => false }
    withClue(escaped(value) + ": ")(accepted must be(expected))
  }

  it should "agree with the JDK UTF-8 validator for every individual UTF-16 code unit" in {
    (0 to 0xffff).foreach(unit => checkAgainstJdk(unit.toChar.toString))
  }

  it should "agree with the JDK on seeded arbitrary UTF-16 sequences" in {
    val random = new Random(99)
    for (_ <- 0 until 1000) {
      val value =
        Array.fill(random.nextInt(64))(random.nextInt(0x10000).toChar).mkString
      checkAgainstJdk(value)
    }
  }

  it should "round-trip seeded valid strings across UTF-8 widths" in {
    // Adapted from protobuf-java Utf8Test.testEncode/randomString: exercise
    // ASCII, two-byte, three-byte, and supplementary code-point ranges with seed 99.
    // https://github.com/protocolbuffers/protobuf/blob/main/java/core/src/test/java/com/google/protobuf/Utf8Test.java
    val random = new Random(99)
    for (limit <- Seq(0x80, 0x90, 0x800, 0x10000, 0x110000); _ <- 0 until 20) {
      val value = new java.lang.StringBuilder()
      for (_ <- 0 until 64) {
        var cp = random.nextInt(limit)
        while (cp >= 0xd800 && cp <= 0xdfff) cp = random.nextInt(limit)
        value.appendCodePoint(cp)
      }
      checkAccepted(value.toString)
    }
  }

  it should "keep the same parser usable after invalid input" in {
    for (_ <- 0 until 3) {
      intercept[JsonFormatException](
        parser.fromJson[StringValue](JString("\ud800"))
      )
      intercept[JsonFormatException](
        parser.fromJsonString[StringValue](quoted("\\" + "udc00"))
      )
      parser.fromJsonString[StringValue]("\"valid\"") must be(
        StringValue("valid")
      )
    }
  }
}
