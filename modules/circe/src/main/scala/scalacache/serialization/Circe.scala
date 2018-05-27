package scalacache.serialization

import akka.util.ByteString
import io.circe.jawn.JawnParser
import io.circe.{Decoder, Encoder}

package object circe {

  private[this] val parser = new JawnParser

  implicit def codec[A](implicit encoder: Encoder[A], decoder: Decoder[A]): Codec[A] = new Codec[A] {

    override def encode(value: A): ByteString = ByteString(encoder.apply(value).noSpaces)

    override def decode(bytes: ByteString): Codec.DecodingResult[A] =
      parser.decodeByteBuffer(bytes.toByteBuffer).left.map(FailedToDecode)

  }

}
