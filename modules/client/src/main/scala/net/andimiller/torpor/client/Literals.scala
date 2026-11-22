package net.andimiller.torpor.client

import io.circe.Decoder

import scala.quoted.*

/** Codecs for a union of string literals, such as `"DOG" | "CAT"`, which is what an enum is read as:
  * {{{
  * type Species = "DOG" | "CAT"
  * given Decoder[Species] = Literals.oneOf
  *
  * final case class Pet(species: Species) derives Decoder
  * }}}
  * The decoder is bound by name rather than found for any union, because a given in scope for every `A <: String` would
  * also be chosen for `String` itself, over circe's own.
  */
object Literals {

  /** Reads one of the literals, and nothing else. */
  inline def oneOf[A <: String]: Decoder[A] = ${ decoder[A] }

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
