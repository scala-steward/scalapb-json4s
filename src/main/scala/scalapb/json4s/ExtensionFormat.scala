package scalapb.json4s

import com.google.protobuf.{
  ByteString,
  Descriptors,
  DynamicMessage,
  ExtensionRegistry,
  Message
}
import com.google.protobuf.descriptor.FieldDescriptorProto
import org.json4s.JsonAST._
import scalapb.{ExtendableMessage, GeneratedMessage, GeneratedMessageCompanion}
import scalapb.descriptors._
import scala.collection.JavaConverters._
import scala.collection.mutable

/** Descriptor-backed extensions declared in the message's file, dependencies,
  * or registered types.
  */
private[json4s] object ExtensionFormat {
  private def fields(
      descriptor: Descriptors.Descriptor,
      types: TypeRegistry
  ): Seq[Descriptors.FieldDescriptor] = {
    val seen = mutable.Set.empty[Descriptors.FileDescriptor]
    val result = Vector.newBuilder[Descriptors.FieldDescriptor]
    def message(d: Descriptors.Descriptor): Unit = {
      result ++= d.getExtensions.asScala
      d.getNestedTypes.asScala.foreach(message)
    }
    def file(f: Descriptors.FileDescriptor): Unit = if (seen.add(f)) {
      result ++= f.getExtensions.asScala
      f.getMessageTypes.asScala.foreach(message)
      f.getDependencies.asScala.foreach(file)
    }
    file(descriptor.getFile)
    types.companions.values.foreach(c => file(c.javaDescriptor.getFile))
    result.result().filter(_.getContainingType == descriptor)
  }

  def find(
      cmp: GeneratedMessageCompanion[_],
      types: TypeRegistry,
      name: String
  ): Option[Descriptors.FieldDescriptor] =
    if (!name.startsWith("[") || !name.endsWith("]")) None
    else
      fields(cmp.javaDescriptor, types).find(f =>
        "[" + f.getFullName + "]" == name
      )

  private def registry(
      fields: Seq[Descriptors.FieldDescriptor]
  ): ExtensionRegistry = {
    val registry = ExtensionRegistry.newInstance()
    fields.foreach { f =>
      if (f.getJavaType == Descriptors.FieldDescriptor.JavaType.MESSAGE)
        registry.add(f, DynamicMessage.getDefaultInstance(f.getMessageType))
      else registry.add(f)
    }
    registry
  }

  private def companion(
      types: TypeRegistry,
      descriptor: Descriptors.Descriptor
  ): JsonFormat.GenericCompanion =
    types
      .findType("type.googleapis.com/" + descriptor.getFullName)
      .getOrElse(
        throw new JsonFormatException(
          s"Register ${descriptor.getFullName} in the TypeRegistry to use its message extension"
        )
      )

  private def parseValue(
      fd: Descriptors.FieldDescriptor,
      value: JValue,
      parser: Parser
  ): AnyRef = {
    import Descriptors.FieldDescriptor.JavaType
    fd.getJavaType match {
      case JavaType.MESSAGE =>
        val cmp = companion(parser.typeRegistry, fd.getMessageType)
        val parsed = parser.fromJson(value)(cmp)
        DynamicMessage.parseFrom(fd.getMessageType, parsed.toByteArray)
      case JavaType.ENUM =>
        val result = value match {
          case JString(name) => Option(fd.getEnumType.findValueByName(name))
          case JInt(n) if n.isValidInt =>
            Option(fd.getEnumType.findValueByNumber(n.toInt))
          case _ => None
        }
        result.getOrElse(
          throw new JsonFormatException(
            s"Invalid enum extension ${fd.getFullName}: $value"
          )
        )
      case _ =>
        JsonFormat.parsePrimitive(
          FieldDescriptorProto.Type.fromValue(fd.toProto.getType.getNumber),
          value,
          throw new JsonFormatException(
            s"Invalid extension ${fd.getFullName}: $value"
          )
        ) match {
          case PInt(n)        => Int.box(n)
          case PLong(n)       => Long.box(n)
          case PFloat(n)      => Float.box(n)
          case PDouble(n)     => Double.box(n)
          case PBoolean(b)    => Boolean.box(b)
          case PString(s)     => s
          case PByteString(b) => b
          case _              =>
            throw new JsonFormatException(
              s"Unsupported extension ${fd.getFullName}"
            )
        }
    }
  }

  private def containsExtension(value: JValue): Boolean = {
    var todo = List(Iterator.single(value))
    while (todo.nonEmpty) {
      if (!todo.head.hasNext) todo = todo.tail
      else
        todo.head.next() match {
          case JObject(fields) =>
            if (fields.exists(_._1.startsWith("["))) return true
            todo = fields.iterator.map(_._2) :: todo
          case JArray(values) => todo = values.iterator :: todo
          case _              =>
        }
    }
    false
  }

  // PMessage does not carry unknown fields. Restore extensions after normal field conversion,
  // including inside nested/repeated messages and maps, before reading the final binary message.
  def restore[A <: GeneratedMessage](
      cmp: GeneratedMessageCompanion[A],
      message: A,
      json: JValue,
      parser: Parser
  ): A = {
    if (!containsExtension(json)) message
    else
      cmp.parseFrom(
        merge(
          cmp.javaDescriptor,
          message.toByteString,
          json,
          parser
        ).toByteArray
      )
  }

  private def merge(
      desc: Descriptors.Descriptor,
      bytes: ByteString,
      json: JValue,
      parser: Parser
  ): Message = {
    val extensions = fields(desc, parser.typeRegistry)
    val builder =
      DynamicMessage.newBuilder(desc).mergeFrom(bytes, registry(extensions))
    json match {
      case JObject(entries) =>
        entries.foreach { case (name, value) =>
          extensions.find(f => name == "[" + f.getFullName + "]") match {
            case Some(fd) if value != JNull =>
              if (fd.isRepeated) value match {
                case JArray(values) =>
                  builder.setField(
                    fd,
                    values.map(parseValue(fd, _, parser)).asJava
                  )
                case _ =>
                  throw new JsonFormatException(
                    s"Expected array for extension ${fd.getFullName}"
                  )
              }
              else builder.setField(fd, parseValue(fd, value, parser))
            case Some(_) =>
            case None    =>
              desc.getFields.asScala
                .find(f => f.getName == name || f.getJsonName == name)
                .foreach { fd =>
                  if (
                    fd.getJavaType == Descriptors.FieldDescriptor.JavaType.MESSAGE && value != JNull
                  ) {
                    if (fd.isRepeated) {
                      val messages = builder
                        .getField(fd)
                        .asInstanceOf[java.util.List[Message]]
                        .asScala
                        .toVector
                      value match {
                        case JArray(values) =>
                          messages.zip(values).zipWithIndex.foreach {
                            case ((m, input), i) =>
                              builder.setRepeatedField(
                                fd,
                                i,
                                merge(
                                  fd.getMessageType,
                                  m.toByteString,
                                  input,
                                  parser
                                )
                              )
                          }
                        case JObject(values) if fd.isMapField =>
                          val byKey = values.toMap
                          val keyField = fd.getMessageType.findFieldByNumber(1)
                          messages.zipWithIndex.foreach { case (m, i) =>
                            val keyValue = scalar(
                              keyField,
                              m.getField(keyField),
                              null,
                              false,
                              false
                            )
                            val key = keyValue match {
                              case JString(s) => s
                              case JInt(n)    => n.toString
                              case JBool(b)   => b.toString
                              case _          =>
                                throw new JsonFormatException("Invalid map key")
                            }
                            byKey.get(key).foreach { input =>
                              builder.setRepeatedField(
                                fd,
                                i,
                                merge(
                                  fd.getMessageType,
                                  m.toByteString,
                                  JObject("value" -> input),
                                  parser
                                )
                              )
                            }
                          }
                        case _ =>
                      }
                    } else if (builder.hasField(fd)) {
                      val m = builder.getField(fd).asInstanceOf[Message]
                      builder.setField(
                        fd,
                        merge(fd.getMessageType, m.toByteString, value, parser)
                      )
                    }
                  }
                }
          }
        }
      case _ =>
    }
    builder.build()
  }

  private def scalar(
      fd: Descriptors.FieldDescriptor,
      value: Any,
      printer: Printer,
      longAsNumber: Boolean,
      enumAsNumber: Boolean
  ): JValue = {
    import Descriptors.FieldDescriptor.{JavaType, Type}
    fd.getJavaType match {
      case JavaType.INT =>
        val n = value.asInstanceOf[Int]
        JInt(
          if (fd.getType == Type.UINT32 || fd.getType == Type.FIXED32)
            BigInt(n & 0xffffffffL)
          else BigInt(n)
        )
      case JavaType.LONG =>
        val n = value.asInstanceOf[Long]
        val number =
          if (
            n < 0 && (fd.getType == Type.UINT64 || fd.getType == Type.FIXED64)
          ) BigInt(n) + (BigInt(1) << 64)
          else BigInt(n)
        if (longAsNumber) JInt(number) else JString(number.toString)
      case JavaType.FLOAT =>
        val n = value.asInstanceOf[Float]
        if (n.isNaN || n.isInfinite) JDouble(n.toDouble)
        else JDecimal(BigDecimal.decimal(n))
      case JavaType.DOUBLE      => JDouble(value.asInstanceOf[Double])
      case JavaType.BOOLEAN     => JBool(value.asInstanceOf[Boolean])
      case JavaType.STRING      => JString(value.asInstanceOf[String])
      case JavaType.BYTE_STRING =>
        JString(
          java.util.Base64.getEncoder
            .encodeToString(value.asInstanceOf[ByteString].toByteArray)
        )
      case JavaType.ENUM =>
        val e = value.asInstanceOf[Descriptors.EnumValueDescriptor]
        if (enumAsNumber) JInt(e.getNumber) else JString(e.getName)
      case JavaType.MESSAGE =>
        val cmp = companion(printer.typeRegistry, fd.getMessageType)
        printer.toJson(cmp.parseFrom(value.asInstanceOf[Message].toByteArray))
    }
  }

  def print(
      message: GeneratedMessage,
      printer: Printer,
      longAsNumber: Boolean,
      enumAsNumber: Boolean
  ): List[JField] = message match {
    case m: ExtendableMessage[_] if m.unknownFields.asMap.nonEmpty =>
      val extensions =
        fields(message.companion.javaDescriptor, printer.typeRegistry)
      if (extensions.isEmpty) Nil
      else {
        val parsed = DynamicMessage.parseFrom(
          message.companion.javaDescriptor,
          message.toByteArray,
          registry(extensions)
        )
        parsed.getAllFields.asScala.toList
          .filter(_._1.isExtension)
          .sortBy(_._1.getNumber)
          .map { case (fd, value) =>
            val json =
              if (fd.isRepeated)
                JArray(
                  value
                    .asInstanceOf[java.util.List[AnyRef]]
                    .asScala
                    .toList
                    .map(scalar(fd, _, printer, longAsNumber, enumAsNumber))
                )
              else scalar(fd, value, printer, longAsNumber, enumAsNumber)
            ("[" + fd.getFullName + "]", json)
          }
      }
    case _ => Nil
  }
}
