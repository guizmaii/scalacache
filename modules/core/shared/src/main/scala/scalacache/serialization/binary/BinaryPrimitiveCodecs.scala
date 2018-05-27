package scalacache.serialization.binary

import akka.util.ByteString
import scalacache.serialization.Codec
import scalacache.serialization.Codec._

import scala.reflect.ClassTag

/**
  * Codecs for all the Java primitive types, plus String and Array[Byte]
  *
  * Credit: Shade @ https://github.com/alexandru/shade/blob/master/src/main/scala/shade/memcached/Codec.scala
  */
trait BinaryPrimitiveCodecs extends LowPriorityBinaryPrimitiveCodecs {

  implicit object IntBinaryCodec extends Codec[Int] {
    override def encode(value: Int): ByteString =
      ByteString(
        (value >>> 24).asInstanceOf[Byte],
        (value >>> 16).asInstanceOf[Byte],
        (value >>> 8).asInstanceOf[Byte],
        value.asInstanceOf[Byte]
      )

    override def decode(data: ByteString): DecodingResult[Int] = tryDecode(
      (data(0).asInstanceOf[Int] & 255) << 24 |
        (data(1).asInstanceOf[Int] & 255) << 16 |
        (data(2).asInstanceOf[Int] & 255) << 8 |
        data(3).asInstanceOf[Int] & 255
    )
  }

}

trait LowPriorityBinaryPrimitiveCodecs extends LowerPriorityBinaryAnyRefCodecs {

  implicit object DoubleBinaryCodec extends Codec[Double] {
    import java.lang.{Double => JvmDouble}
    override def encode(value: Double): ByteString = {
      val l = JvmDouble.doubleToLongBits(value)
      LongBinaryCodec.encode(l)
    }

    override def decode(data: ByteString): DecodingResult[Double] = {
      LongBinaryCodec
        .decode(data)
        .right
        .map(l => JvmDouble.longBitsToDouble(l))
    }
  }

  implicit object FloatBinaryCodec extends Codec[Float] {
    import java.lang.{Float => JvmFloat}
    override def encode(value: Float): ByteString = {
      val i = JvmFloat.floatToIntBits(value)
      IntBinaryCodec.encode(i)
    }

    override def decode(data: ByteString): DecodingResult[Float] = {
      IntBinaryCodec
        .decode(data)
        .right
        .map(i => JvmFloat.intBitsToFloat(i))
    }
  }

  implicit object LongBinaryCodec extends Codec[Long] {
    override def encode(value: Long): ByteString =
      ByteString(
        (value >>> 56).asInstanceOf[Byte],
        (value >>> 48).asInstanceOf[Byte],
        (value >>> 40).asInstanceOf[Byte],
        (value >>> 32).asInstanceOf[Byte],
        (value >>> 24).asInstanceOf[Byte],
        (value >>> 16).asInstanceOf[Byte],
        (value >>> 8).asInstanceOf[Byte],
        value.asInstanceOf[Byte]
      )

    override def decode(data: ByteString): DecodingResult[Long] = tryDecode(
      (data(0).asInstanceOf[Long] & 255) << 56 |
        (data(1).asInstanceOf[Long] & 255) << 48 |
        (data(2).asInstanceOf[Long] & 255) << 40 |
        (data(3).asInstanceOf[Long] & 255) << 32 |
        (data(4).asInstanceOf[Long] & 255) << 24 |
        (data(5).asInstanceOf[Long] & 255) << 16 |
        (data(6).asInstanceOf[Long] & 255) << 8 |
        data(7).asInstanceOf[Long] & 255
    )
  }

  implicit object BooleanBinaryCodec extends Codec[Boolean] {
    override def encode(value: Boolean): ByteString =
      ByteString((if (value) 1 else 0).asInstanceOf[Byte])

    override def decode(data: ByteString): DecodingResult[Boolean] =
      tryDecode(data.isDefinedAt(0) && data(0) == 1)
  }

  implicit object CharBinaryCodec extends Codec[Char] {
    override def encode(value: Char): ByteString = ByteString(
      (value >>> 8).asInstanceOf[Byte],
      value.asInstanceOf[Byte]
    )

    override def decode(data: ByteString): DecodingResult[Char] = tryDecode(
      ((data(0).asInstanceOf[Int] & 255) << 8 |
        data(1).asInstanceOf[Int] & 255)
        .asInstanceOf[Char]
    )
  }

  implicit object ShortBinaryCodec extends Codec[Short] {
    override def encode(value: Short): ByteString = ByteString(
      (value >>> 8).asInstanceOf[Byte],
      value.asInstanceOf[Byte]
    )

    override def decode(data: ByteString): DecodingResult[Short] = tryDecode(
      ((data(0).asInstanceOf[Short] & 255) << 8 |
        data(1).asInstanceOf[Short] & 255)
        .asInstanceOf[Short]
    )
  }

  implicit object StringBinaryCodec extends Codec[String] {
    override def encode(value: String): ByteString = ByteString(value)
    override def decode(data: ByteString): DecodingResult[String] = tryDecode(data.utf8String)
  }

  implicit object ArrayByteBinaryCodec extends Codec[Array[Byte]] {
    override def encode(value: Array[Byte]): ByteString = ByteString(value)
    override def decode(data: ByteString): DecodingResult[Array[Byte]] = Right(data.toArray)
  }
}

trait LowerPriorityBinaryAnyRefCodecs {

  /**
    * String and Array[Byte] extend java.io.Serializable,
    * so this implicit needs to be lower priority than those in BinaryPrimitiveCodecs
    */
  implicit def anyRefBinaryCodec[S <: java.io.Serializable](implicit ev: ClassTag[S]): Codec[S] =
    new JavaSerializationAnyRefCodec[S](ev)

}
