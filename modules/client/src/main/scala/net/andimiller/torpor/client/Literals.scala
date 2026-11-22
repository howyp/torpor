package net.andimiller.torpor.client

import io.circe.Decoder

import scala.quoted.*

/** Codecs for a union of string literals, such as `"DOG" | "CAT"`, which is what an enum is read as:
  * {{{
  * import net.andimiller.torpor.client.Literals.given
  *
  * final case class Pet(species: "DOG" | "CAT") derives Decoder
  * }}}
  */
object Literals {

  /** Reads one of the literals, and nothing else. */
  inline given oneOf[A <: String]: Decoder[A] = ${ decoder[A] }

  private def decoder[A <: String: Type](using Quotes): Expr[Decoder[A]] = {
    import quotes.reflect.*
    val values   = MacroSupport
      .literals(TypeRepr.of[A])
      .getOrElse(report.errorAndAbort(s"${TypeRepr.of[A].show} is not a union of string literals"))
    val expected = Expr(values)
    '{
      Decoder.decodeString.emap { value =>
        if ($expected.contains(value)) Right(value.asInstanceOf[A])
        else Left(s"$value is not one of ${$expected.mkString(", ")}")
      }
    }
  }
}
