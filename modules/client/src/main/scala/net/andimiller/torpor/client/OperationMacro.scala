package net.andimiller.torpor.client

import scala.quoted.*

object OperationMacro {

  def impl[V: Type, D: Type](schemaFile: Expr[String], document: Expr[String])(using
      Quotes
  ): Expr[Operation[V, D]] = {
    import quotes.reflect.*

    val schema = MacroSupport.schema(MacroSupport.schemaFile(schemaFile))
    val query  = MacroSupport.document(document)

    Check.operation(schema, query, MacroSupport.shape(TypeRepr.of[V]), MacroSupport.shape(TypeRepr.of[D])) match {
      case Left(problems) =>
        report.errorAndAbort(problems.mkString("the operation does not fit the schema or its types:\n  ", "\n  ", ""))
      case Right(name)    =>
        '{ Operation.unchecked[V, D](${ Expr(query) }, ${ Expr(name) }) }
    }
  }
}
