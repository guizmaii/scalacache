package scalacache.serialization

import akka.util.{ByteString, CompactByteString}

import scala.annotation.implicitNotFound
import scala.language.implicitConversions
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/**
  * Represents a type class that needs to be implemented
  * for serialization/deserialization to work.
  */
@implicitNotFound(msg = """Could not find any Codecs for type ${A}.
If you would like to serialize values in a binary format, please import the binary codec:

import scalacache.serialization.binary._

If you would like to serialize values as JSON using circe, please import the circe codec
and provide a circe Encoder[${A}] and Decoder[${A}], e.g.:

import scalacache.serialization.circe._
import io.circe.generic.auto._

You will need a dependency on the scalacache-circe module.

See the documentation for more details on codecs.""")
trait Codec[A] {
  def encode(value: A): ByteString
  def decode(bytes: ByteString): Codec.DecodingResult[A]
}

/**
  * For simple primitives, we provide lightweight Codecs for ease of use.
  */
object Codec {

  type DecodingResult[A] = Either[FailedToDecode, A]

  object DecodingResult {
    def toOption[A](decodingResult: DecodingResult[A]): Option[A] = decodingResult.fold(_ => None, Some.apply)
  }

  def tryDecode[A](f: => A): DecodingResult[A] =
    try Right(f)
    catch {
      case NonFatal(e) => Left(FailedToDecode(e))
    }

}
