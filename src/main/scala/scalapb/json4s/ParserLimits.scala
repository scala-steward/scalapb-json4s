package scalapb.json4s

import com.fasterxml.jackson.core.{JsonFactory, JsonToken}
import org.json4s.JsonAST._

/** Resource checks performed before recursive JSON/protobuf conversion. */
private[json4s] object ParserLimits {
  val MaxNumberLength = 1024
  private val factory = new JsonFactory()

  // Character's predicates validate UTF-16 without allocating an encoded copy.
  // UTF-8 CharsetEncoder.canEncode is used as an independent oracle in tests.
  private def checkUnicode(value: String): Unit = {
    var i = 0
    while (i < value.length) {
      val ch = value.charAt(i)
      if (Character.isSurrogate(ch)) {
        if (
          i + 1 >= value.length || !Character.isSurrogatePair(
            ch,
            value.charAt(i + 1)
          )
        )
          throw new JsonFormatException("Unpaired surrogate in JSON string")
        i += 2
      } else i += 1
    }
  }

  def checkNumberLength(length: Int): Unit =
    if (length > MaxNumberLength)
      throw new JsonFormatException("Numeric literal exceeds 1024 characters")

  private def checkDepth(depth: Int, limit: Int): Unit =
    if (depth > limit)
      throw new JsonFormatException("JSON nesting limit exceeded")

  // Jackson's streaming reader does not construct BigInt/BigDecimal values. Check
  // before json4s builds its recursively allocated tree and arbitrary-precision numbers.
  def checkString(input: String, limit: Int): Unit = {
    val reader = factory.createParser(input)
    try {
      var depth = 0
      var token = reader.nextToken()
      while (token != null) {
        token match {
          case JsonToken.START_OBJECT | JsonToken.START_ARRAY =>
            depth += 1
            checkDepth(depth, limit)
          case JsonToken.END_OBJECT | JsonToken.END_ARRAY => depth -= 1
          case JsonToken.VALUE_NUMBER_INT | JsonToken.VALUE_NUMBER_FLOAT =>
            checkNumberLength(reader.getTextLength)
          case JsonToken.VALUE_STRING | JsonToken.FIELD_NAME =>
            checkUnicode(reader.getText)
          case _ =>
        }
        token = reader.nextToken()
      }
    } catch {
      case e: JsonFormatException                                => throw e
      case e: com.fasterxml.jackson.core.JsonProcessingException =>
        throw new JsonFormatException("Invalid JSON input", e)
    } finally reader.close()
  }

  // Iterators keep both stack use and auxiliary memory bounded by depth, not width.
  def checkValue(value: JValue, limit: Int): Unit = {
    var stack = List((Iterator.single(value), 0))
    while (stack.nonEmpty) {
      val (values, depth) = stack.head
      if (!values.hasNext) stack = stack.tail
      else {
        val children = values.next() match {
          case JObject(fields) =>
            fields.foreach(f => checkUnicode(f._1))
            Some(fields.iterator.map(_._2))
          case JString(value) =>
            checkUnicode(value)
            None
          case JArray(values) => Some(values.iterator)
          case JSet(values)   => Some(values.iterator)
          case JInt(n)        =>
            // 1024 decimal digits require at most 3402 bits. Avoid converting
            // arbitrarily large caller-supplied integers to decimal strings.
            if (n.bitLength > 3402) checkNumberLength(MaxNumberLength + 1)
            checkNumberLength(n.toString.length)
            None
          case JDecimal(n) =>
            checkNumberLength(n.bigDecimal.precision())
            None
          case _ => None
        }
        children.foreach { it =>
          checkDepth(depth + 1, limit)
          stack = (it, depth + 1) :: stack
        }
      }
    }
  }
}
