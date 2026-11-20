package net.andimiller.torpor.client

import io.circe.Decoder

import scala.quoted.*

/** A GraphQL API as its caller sees it: the schema file, and the Scala type each leaf is read as.
  *
  * {{{
  * val petshop = Api
  *   .at("schema.graphqls")
  *   .read["Pet.id", PetId]   // one field
  *   .read["Int", Long]       // every field of a type
  *
  * val pets = petshop.operation("query Pets($first: Int) { pets(first: $first) { id name } }")
  * }}}
  *
  * `read` names a field as `Type.field` or a scalar or enum by its name, and takes the `Decoder` in scope for the Scala
  * type, or one passed explicitly. A field's own binding is used before its type's. Unbound, `Int` is `Int`, `Float` is
  * `Double`, `String`, `ID` and an enum are `String`, and any other scalar is circe `Json`.
  *
  * The file and the bindings are part of this value's type, which is where `operation` reads them from. So nothing has
  * to be in scope where an operation is stated but the `Api` itself.
  */
final class Api[File <: String, Bindings <: Tuple] private (val decoders: Map[String, Decoder[?]]) {

  /** Reads the named field, scalar or enum as `A`. */
  def read[Name <: String & Singleton, A](using
      name: ValueOf[Name],
      decoder: Decoder[A]
  ): Api[File, (Name, A) *: Bindings]                                                =
    new Api(decoders + (name.value -> decoder))

  /** An operation typed from the schema: see [[TypedOperation]]. Its response is a [[Record]] with a member for each
    * field the document selects, and its variables are given by name.
    */
  transparent inline def operation(inline document: String): TypedOperation[?, ?, ?] =
    ${ TypedMacro.typed[File, Bindings]('this, 'document) }
}

object Api {

  /** The API whose schema is the SDL in `file`, such as the one `torpor-testkit` writes. The file is looked for from
    * the working directory, then from each directory above the source file that states an operation.
    */
  def at[File <: String & Singleton](file: File): Api[File, EmptyTuple] = new Api(Map.empty)
}
