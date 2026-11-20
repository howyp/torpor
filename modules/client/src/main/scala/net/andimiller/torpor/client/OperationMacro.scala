package net.andimiller.torpor.client

import java.nio.file.{Files, Path, Paths}

import net.andimiller.torpor.schema.{Schema, SdlFragment}

import scala.quoted.*

object OperationMacro {

  def impl[V: Type, D: Type](schemaFile: Expr[String], document: Expr[String])(using
      Quotes
  ): Expr[Operation[V, D]] = {
    import quotes.reflect.*

    val file  = schemaFile.value.getOrElse(report.errorAndAbort("the schema file must be a literal string"))
    val query = document.value.getOrElse(report.errorAndAbort("the operation must be a literal string"))

    val calledFrom = Position.ofMacroExpansion.sourceFile.getJPath.map(_.toAbsolutePath)
    val schema     = read(file, calledFrom) match {
      case Left(problem) => report.errorAndAbort(problem)
      case Right(sdl)    =>
        Schema.merge(List(SdlFragment(file, sdl))) match {
          case Left(errors)  => report.errorAndAbort(errors.toChain.toList.map(_.render).mkString("\n"))
          case Right(schema) => schema
        }
    }

    val optionSymbol   = TypeRepr.of[Option[Any]].typeSymbol
    val iterableSymbol = TypeRepr.of[Iterable[Any]].typeSymbol
    // collections that are not an `Iterable`, by the name of their class
    val collections    = Set("NonEmptyList", "NonEmptyVector", "NonEmptyChain", "NonEmptySeq", "Chain")

    def shape(tpe: TypeRepr, within: Set[String]): Shape = {
      val dealiased = tpe.dealias
      val symbol    = dealiased.typeSymbol
      dealiased match {
        case AppliedType(_, List(of)) if symbol == optionSymbol                                       =>
          Shape.Optional(shape(of, within))
        case AppliedType(_, List(of)) if collections(symbol.name)                                     =>
          Shape.Many(shape(of, within))
        case _ if dealiased <:< TypeRepr.of[Iterable[Any]] && !(dealiased <:< TypeRepr.of[Map[?, ?]]) =>
          dealiased.baseType(iterableSymbol) match {
            case AppliedType(_, List(of)) => Shape.Many(shape(of, within))
            case _                        => Shape.Leaf(symbol.name)
          }
        // a recursive case class is checked once: beneath itself it is taken on trust
        case _ if symbol.isClassDef && symbol.flags.is(Flags.Case) && !within(symbol.fullName)        =>
          Shape.Record(
            symbol.name,
            symbol.caseFields.map(field => field.name -> shape(dealiased.memberType(field), within + symbol.fullName))
          )
        case _                                                                                        =>
          Shape.Leaf(symbol.name)
      }
    }

    Check.operation(schema, query, shape(TypeRepr.of[V], Set.empty), shape(TypeRepr.of[D], Set.empty)) match {
      case Left(problems) =>
        report.errorAndAbort(problems.mkString("the operation does not fit the schema or its types:\n  ", "\n  ", ""))
      case Right(name)    =>
        '{ Operation.unchecked[V, D](${ Expr(query) }, ${ Expr(name) }) }
    }
  }

  /** The schema's text. A relative path is tried from the working directory, which under sbt is the build's root, and
    * then from each directory above the calling source file, which is where an editor's compiler finds it.
    */
  private def read(file: String, calledFrom: Option[Path]): Either[String, String] = {
    val asGiven    = Paths.get(file)
    val ancestors  =
      calledFrom.toList.flatMap(source => Iterator.iterate(source.getParent)(_.getParent).takeWhile(_ != null))
    val candidates = (asGiven :: ancestors.map(_.resolve(file))).filter(Files.isRegularFile(_))
    candidates.headOption match {
      case Some(found) => Right(new String(Files.readAllBytes(found), "UTF-8"))
      case None        =>
        Left(
          s"cannot find the schema file '$file' from the working directory or any directory above the calling source"
        )
    }
  }
}
