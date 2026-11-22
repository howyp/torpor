package net.andimiller.torpor.client

import net.andimiller.torpor.schema.Schema

import scala.quoted.*

object OperationMacro {

  def fromFile[V: Type, D: Type](schemaFile: Expr[String], document: Expr[String])(using
      Quotes
  ): Expr[Operation[V, D]] =
    impl[V, D](MacroSupport.schemaInFile(MacroSupport.schemaFile(schemaFile)), document)

  def fromSdl[V: Type, D: Type](sdl: Expr[String], document: Expr[String])(using Quotes): Expr[Operation[V, D]] =
    impl[V, D](MacroSupport.schemaInSdl(MacroSupport.schemaText(sdl)), document)

  private def impl[V: Type, D: Type](schema: Schema, document: Expr[String])(using Quotes): Expr[Operation[V, D]] = {
    import quotes.reflect.*

    val query = MacroSupport.document(document)

    Check.operation(schema, query, MacroSupport.shape(TypeRepr.of[V]), MacroSupport.shape(TypeRepr.of[D])) match {
      case Left(problems) =>
        report.errorAndAbort(problems.mkString("the operation does not fit the schema or its types:\n  ", "\n  ", ""))
      case Right(name)    =>
        '{ Operation.unchecked[V, D](${ Expr(query) }, ${ Expr(name) }) }
    }
  }
}
