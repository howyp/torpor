package net.andimiller.torpor.client

import io.circe.Decoder

import scala.quoted.*

/** A GraphQL API as its caller sees it: the schema, and the Scala type each leaf is read as.
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
  * `Double`, `String` and `ID` are `String`, an enum is the union of its values as literals, and any other scalar is
  * circe `Json`.
  *
  * The schema and the bindings are part of this value's type, which is where `operation` reads them from. So nothing
  * has to be in scope where an operation is stated but the `Api` itself.
  */
final class Api[Source <: Api.Source, Bindings <: Tuple] private (val decoders: Map[String, Decoder[?]]) {

  /** Reads the named field, scalar or enum as `A`. */
  def read[Name <: String & Singleton, A](using
      name: ValueOf[Name],
      decoder: Decoder[A]
  ): Api[Source, (Name, A) *: Bindings]                                              =
    new Api(decoders + (name.value -> decoder))

  /** An operation typed from the schema: see [[TypedOperation]]. Its response is a [[Record]] with a member for each
    * field the document selects, and its variables are given by name.
    */
  transparent inline def operation(inline document: String): TypedOperation[?, ?, ?] =
    ${ TypedMacro.typed[Source, Bindings]('this, 'document) }

  override def toString: String = decoders.keys.toList.sorted.mkString("Api(", ", ", ")")
}

object Api {

  /** Where the schema comes from, as part of an `Api` value's type. */
  sealed trait Source

  /** An SDL file, found from the working directory or from any directory above the source file that states an
    * operation.
    */
  sealed trait File[Path <: String] extends Source

  /** The SDL itself, as a constant: see [[sdl]]. */
  sealed trait Sdl[Text <: String] extends Source

  /** The API whose schema is the SDL in `file`, such as the one `torpor-testkit` writes. The file is looked for from
    * the working directory, then from each directory above the source file that states an operation.
    *
    * The compiler does not know an operation depends on the file, so editing the file does not recompile the operations
    * stated against it until something else does: see [[sdl]] for a schema the compiler tracks.
    */
  def at[Path <: String & Singleton](file: Path): Api[File[Path], EmptyTuple] = new Api(Map.empty)

  /** The API whose schema is the given SDL: a literal, or an `inline val` such as one the build generates from the
    * schema file. A Scala source is something the compiler tracks, so every operation stated against the schema
    * recompiles when it changes.
    */
  transparent inline def sdl(inline text: String): Api[?, EmptyTuple] = ${ ApiMacro.sdl('text) }

  /** Called by code `sdl` generates; not part of the API. */
  def unchecked[S <: Source]: Api[S, EmptyTuple] = new Api(Map.empty)
}

private[client] object ApiMacro {

  def sdl(text: Expr[String])(using Quotes): Expr[Api[?, EmptyTuple]] = {
    import quotes.reflect.*
    val constant = MacroSupport
      .constant(text)
      .getOrElse(report.errorAndAbort("the schema must be a constant string: a literal, or an inline val"))
    TypeRepr.of[Api.Sdl].appliedTo(ConstantType(StringConstant(constant))).asType match {
      case '[s] => '{ Api.unchecked[s & Api.Source] }
    }
  }
}
