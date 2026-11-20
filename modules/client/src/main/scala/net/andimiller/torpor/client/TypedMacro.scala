package net.andimiller.torpor.client

import io.circe.{Decoder, Encoder, Json, JsonObject}
import net.andimiller.torpor.core.Scalar
import net.andimiller.torpor.schema.{Executable, Selection, TypeKind, TypeRef as GqlType}

import scala.quoted.*

/** The macros behind `Operation.typed`, calling a typed operation, and `input(…)`. */
object TypedMacro {

  /** Types an operation from the schema: a [[Record]] refined with the fields the document selects, a decoder for it,
    * and the variables the document declares.
    *
    * A GraphQL type becomes a Scala type the way the server's `schema` macro types arguments: nullable is `Option`, a
    * list is `List`, an object is a nested record. A field selected in a fragment only some types match, or behind
    * `@skip` or `@include`, is an `Option` too. A leaf is read as the type a `Scalar` binding in scope names, looked up
    * first for the field (`Scalar["Pet.id", PetId]`) and then for its type (`Scalar["ID", Long]`). Unbound, `Int` is
    * `Int`, `Float` is `Double`, `String`, `ID` and an enum are `String`, and any other scalar is circe `Json`.
    */
  def typed(schemaFile: Expr[String], document: Expr[String])(using Quotes): Expr[TypedOperation[?, ?, ?]] = {
    import quotes.reflect.*

    val file   = MacroSupport.schemaFile(schemaFile)
    val schema = MacroSupport.schema(file)
    val query  = MacroSupport.document(document)

    val validated = Executable.validate(schema, query) match {
      case Left(errors) =>
        report.errorAndAbort(
          errors.toChain.toList.map(_.render).mkString("the operation does not fit the schema:\n  ", "\n  ", "")
        )
      case Right(valid) => valid
    }
    val check     = new Check(schema, validated.document)

    def literal(value: String): TypeRepr = ConstantType(StringConstant(value))

    def tuple(elements: List[TypeRepr]): TypeRepr =
      elements.foldRight(TypeRepr.of[EmptyTuple])((element, rest) => TypeRepr.of[*:].appliedTo(List(element, rest)))

    def bound(name: String): Option[TypeRepr] =
      Implicits.search(TypeRepr.of[Scalar].appliedTo(List(literal(name), TypeBounds.empty))) match {
        case found: ImplicitSearchSuccess =>
          found.tree.tpe.widen.baseType(TypeRepr.of[Scalar].typeSymbol) match {
            case AppliedType(_, List(_, scala)) => Some(scala)
            case _                              => None
          }
        case _                            => None
      }

    /** The type a leaf is read as, and the decoder in scope for it. */
    def leaf(field: Check.Field, name: String, path: String): (TypeRepr, Expr[Decoder[?]]) = {
      val tpe = bound(s"${field.owner}.${field.name}").orElse(bound(name)).getOrElse {
        name match {
          case "Int"                                                           => TypeRepr.of[Int]
          case "Float"                                                         => TypeRepr.of[Double]
          case "Boolean"                                                       => TypeRepr.of[Boolean]
          case "String" | "ID"                                                 => TypeRepr.of[String]
          case _ if schema.types.get(name).exists(_.kind == TypeKind.EnumType) => TypeRepr.of[String]
          case _                                                               => TypeRepr.of[Json]
        }
      }
      tpe.asType match {
        case '[t] =>
          Expr.summon[Decoder[t]] match {
            case Some(decoder) => (tpe, decoder)
            case None          =>
              report.errorAndAbort(
                s"$path is read as ${tpe.show}, which has no Decoder in scope. It is ${field.owner}.${field.name}: $name in the schema."
              )
          }
      }
    }

    def record(parent: String, selections: List[(String, Selection)], path: String): (TypeRepr, Expr[Decoder[?]]) = {
      val fields  = check.fields(parent, selections).map { field =>
        val (tpe, decoder) = value(field, field.tpe, field.conditional, s"$path.${field.key}")
        (field.key, tpe, decoder)
      }
      val refined = fields.foldLeft(TypeRepr.of[Record]) { case (record, (key, tpe, _)) =>
        Refinement(record, key, tpe)
      }
      val pairs   = Expr.ofList(fields.map { case (key, _, decoder) => '{ (${ Expr(key) }, $decoder) } })
      (refined, '{ Record.decoder($pairs) })
    }

    def value(field: Check.Field, tpe: GqlType, conditional: Boolean, path: String): (TypeRepr, Expr[Decoder[?]]) =
      tpe match {
        case GqlType.NonNull(of) if !conditional => required(field, of, path)
        case other                               =>
          val (inner, decoder) = required(field, other, path)
          (
            TypeRepr.of[Option].appliedTo(inner),
            '{ Decoder.decodeOption(using $decoder.asInstanceOf[Decoder[Any]]) }
          )
      }

    def required(field: Check.Field, tpe: GqlType, path: String): (TypeRepr, Expr[Decoder[?]]) =
      tpe match {
        case GqlType.NonNull(of) => required(field, of, path)
        case GqlType.ListOf(of)  =>
          val (element, decoder) = value(field, of, conditional = false, s"$path[]")
          (TypeRepr.of[List].appliedTo(element), '{ Decoder.decodeList(using $decoder.asInstanceOf[Decoder[Any]]) })
        case GqlType.Named(name) =>
          schema.types.get(name).map(_.kind) match {
            case Some(TypeKind.ObjectType | TypeKind.InterfaceType | TypeKind.UnionType) =>
              record(name, field.selections, path)
            case _                                                                       => leaf(field, name, path)
          }
      }

    val operation = validated.operation
    val root      = check.root(operation).getOrElse(report.errorAndAbort(s"the schema has no ${operation.kind.label} type"))

    val (data, decoder) = record(root, operation.selections.map(root -> _), operation.kind.label)
    val variables       = tuple(operation.variables.map { variable =>
      TypeRepr
        .of[Tuple3]
        .appliedTo(
          List(
            literal(variable.name),
            literal(Check.render(variable.tpe)),
            ConstantType(BooleanConstant(variable.default.isDefined))
          )
        )
    })

    // built untyped and then given its type, which is what a `transparent inline` caller sees
    val untyped = '{
      new TypedOperation[String, Tuple, Any](
        ${ Expr(query) },
        ${ Expr(operation.name) },
        $decoder.asInstanceOf[Decoder[Any]]
      )
    }
    TypeRepr.of[TypedOperation].appliedTo(List(literal(file), variables, data)).asType match {
      case '[t] => '{ $untyped.asInstanceOf[t] }.asExprOf[TypedOperation[?, ?, ?]]
    }
  }

  /** `operation(name = value, …)`: each value against the variable it is given for, then the request. */
  def request[File <: String: Type, Variables <: Tuple: Type, D: Type](
      operation: Expr[TypedOperation[File, Variables, D]],
      supplied: Expr[Seq[(String, Any)]]
  )(using Quotes): Expr[Request[D]] = {
    requestOf[File, Variables, D](operation, named(supplied))
  }

  private def requestOf[File <: String: Type, Variables <: Tuple: Type, D: Type](using q: Quotes)(
      operation: Expr[TypedOperation[File, Variables, D]],
      values: List[(String, q.reflect.TypeRepr, Expr[Json])]
  ): Expr[Request[D]] = {
    import q.reflect.*

    val schema   = MacroSupport.schema(literalOf[File])
    val check    = new Check(schema, net.andimiller.torpor.schema.ExecutableDocument(Nil, Nil))
    val declared = declaredVariables[Variables]

    val unknown  = values.map(_._1).filterNot(name => declared.exists(_._1 == name)).map { name =>
      s"the operation declares no variable '$$$name'"
    }
    val problems = unknown ++ declared.flatMap { case (name, tpe, hasDefault) =>
      check.input(tpe, hasDefault, values.find(_._1 == name).map(value => MacroSupport.shape(value._2)), s"$$$name")
    }
    if (problems.nonEmpty)
      report.errorAndAbort(problems.mkString("the variables do not fit the operation:\n  ", "\n  ", ""))

    val fields = Expr.ofList(values.map { case (name, _, json) => '{ (${ Expr(name) }, $json) } })
    '{
      new Request[D](
        $operation.document,
        $operation.name,
        // a variable given as `None` is left out, which is what leaving it out of the call means
        JsonObject.fromIterable($fields.filterNot(_._2.isNull)),
        $operation.decoder
      )
    }
  }

  /** `operation()`: allowed when no variable must be given a value. */
  def requestWithout[File <: String: Type, Variables <: Tuple: Type, D: Type](
      operation: Expr[TypedOperation[File, Variables, D]],
      supplied: Expr[Seq[Any]]
  )(using Quotes): Expr[Request[D]] = {
    import quotes.reflect.*

    supplied match {
      case Varargs(values) if values.nonEmpty =>
        report.errorAndAbort("give an operation's variables by name: operation(name = value, …)")
      case _                                  => ()
    }
    requestOf[File, Variables, D](operation, Nil)
  }

  /** `input(name = value, …)`: an [[InputObject]] whose type records each field and the type of its value. */
  def input(fields: Expr[Seq[(String, Any)]])(using Quotes): Expr[InputObject[?]] = {
    import quotes.reflect.*

    val values = named(fields)
    val spec   = values.foldRight(TypeRepr.of[EmptyTuple]) { case ((name, tpe, _), rest) =>
      TypeRepr
        .of[*:]
        .appliedTo(List(TypeRepr.of[Tuple2].appliedTo(List(ConstantType(StringConstant(name)), tpe)), rest))
    }
    val pairs  = Expr.ofList(values.map { case (name, _, json) => '{ (${ Expr(name) }, $json) } })

    TypeRepr.of[InputObject].appliedTo(spec).asType match {
      case '[t] => '{ InputObject.of[Tuple]($pairs).asInstanceOf[t] }.asExprOf[InputObject[?]]
    }
  }

  /** Named arguments as `(name, the value's own type, the value encoded)`. */
  private def named(using
      q: Quotes
  )(supplied: Expr[Seq[(String, Any)]]): List[(String, q.reflect.TypeRepr, Expr[Json])] = {
    import q.reflect.*

    val pairs = supplied match {
      case Varargs(pairs) => pairs.toList
      case _              => report.errorAndAbort("give each value by name: name = value")
    }
    pairs.map {
      case '{ ($name: String, $value: Any) } =>
        val key = name.value.filter(_.nonEmpty).getOrElse(report.errorAndAbort("give each value by name: name = value"))
        // the pair is typed `(String, Any)`, so the value's own type is read off its tree
        val tpe = unascribed(value.asTerm).tpe.widen
        tpe.asType match {
          case '[t] =>
            Expr.summon[Encoder[t]] match {
              case Some(encoder) => (key, tpe, '{ $encoder(${ value.asExprOf[t] }) })
              case None          => report.errorAndAbort(s"'$key' is a ${tpe.show}, which has no Encoder in scope")
            }
        }
      case other                             =>
        report.errorAndAbort(s"give each value by name: name = value (found ${other.show})")
    }
  }

  private def unascribed(using q: Quotes)(term: q.reflect.Term): q.reflect.Term = {
    import q.reflect.*
    term match {
      case Inlined(_, Nil, inner) => unascribed(inner)
      case Typed(inner, _)        => unascribed(inner)
      case other                  => other
    }
  }

  private def literalOf[S <: String: Type](using Quotes): String = {
    import quotes.reflect.*
    TypeRepr.of[S].dealias match {
      case ConstantType(StringConstant(value)) => value
      case other                               => report.errorAndAbort(s"expected a literal string type, found ${other.show}")
    }
  }

  private def declaredVariables[V <: Tuple: Type](using Quotes): List[(String, GqlType, Boolean)] = {
    import quotes.reflect.*

    def each(tpe: TypeRepr): List[(String, GqlType, Boolean)] = tpe.dealias match {
      case AppliedType(_, List(AppliedType(_, List(name, declared, default)), rest)) =>
        (name.dealias, declared.dealias, default.dealias) match {
          case (
                ConstantType(StringConstant(variable)),
                ConstantType(StringConstant(gql)),
                ConstantType(BooleanConstant(hasDefault))
              ) =>
            (variable, Check.parse(gql), hasDefault) :: each(rest)
          case _ => each(rest)
        }
      case _                                                                         => Nil
    }

    each(TypeRepr.of[V])
  }
}
