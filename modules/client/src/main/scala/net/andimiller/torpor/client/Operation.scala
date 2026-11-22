package net.andimiller.torpor.client

import scala.quoted.*

/** A GraphQL operation that has been checked against a schema: its variables are written as `V` and its response is
  * read as `D`.
  *
  * {{{
  * final case class PetsVariables(first: Option[Int]) derives Encoder.AsObject
  * final case class Pets(pets: List[Pet]) derives Decoder
  * final case class Pet(name: String, owner: Option[Owner]) derives Decoder
  * final case class Owner(name: String) derives Decoder
  *
  * val pets = Operation.checked[PetsVariables, Pets](
  *   "schema.graphqls",
  *   """query Pets($first: Int) { pets(first: $first) { name owner { name } } }"""
  * )
  * }}}
  *
  * This compiles only if the document is valid against the SDL in that file, and `PetsVariables` and `Pets` have the
  * shape the document gives them: see [[Check]] for the rules. The types are ordinary case classes with whatever codecs
  * they like; nothing is generated.
  */
final class Operation[V, D] private[client] (val document: String, val name: Option[String])

object Operation {

  /** `schemaFile` is the schema as SDL, such as the file `torpor-testkit` writes. It is looked for from the working
    * directory, then from each directory above the source file that calls this. A service with many operations wraps
    * this once, so the path is written in one place:
    * {{{
    * inline def operation[V, D](inline document: String): Operation[V, D] =
    *   Operation.checked[V, D]("src/main/resources/schema.graphqls", document)
    * }}}
    */
  inline def checked[V, D](inline schemaFile: String, inline document: String): Operation[V, D] =
    ${ OperationMacro.fromFile[V, D]('schemaFile, 'document) }

  /** As [[checked]], with the schema as SDL rather than as a file: a literal, or an `inline val` such as one the build
    * generates from the schema file. The compiler tracks a Scala source, so every operation checked against the schema
    * recompiles when it changes, which editing a file does not bring about.
    */
  inline def checkedAgainst[V, D](inline sdl: String, inline document: String): Operation[V, D] =
    ${ OperationMacro.fromSdl[V, D]('sdl, 'document) }

  /** Called by code `checked` generates; not part of the API. */
  def unchecked[V, D](document: String, name: Option[String]): Operation[V, D] = new Operation(document, name)
}
