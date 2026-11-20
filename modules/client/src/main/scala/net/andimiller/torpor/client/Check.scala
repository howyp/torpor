package net.andimiller.torpor.client

import net.andimiller.torpor.schema.{
  Executable,
  ExecutableDocument,
  FieldSelection,
  FragmentSpread,
  InlineFragment,
  OperationDefinition,
  OperationKind,
  Schema,
  Selection,
  TypeKind,
  TypeRef
}

/** Checks an operation against the schema, and the types it is read and written as against the operation.
  *
  * The rules, for the type the response is read as:
  *   - an object is a case class whose fields are exactly the response keys the document selects, so a field that is
  *     read but not selected, or selected but never read, is an error;
  *   - a nullable field is an `Option`; so is a field selected inside a fragment that only some types match, or behind
  *     `@skip` or `@include`, because the response may not carry it;
  *   - a list is a collection;
  *   - `String`, `Boolean`, `Int` and `Float` are read as the Scala types that hold them. Every other scalar and every
  *     enum may be read as any type that is not a case class or a collection: its decoder decides.
  *
  * And for the type the variables are written as: a case class with one field per variable the operation declares,
  * where a variable that must be given a value is not an `Option`. An operation with no variables takes `Unit`.
  */
object Check {

  /** Every problem found, or the name of the operation when there is none. */
  def operation(schema: Schema, document: String, variables: Shape, data: Shape): Either[List[String], Option[String]] =
    Executable.validate(schema, document) match {
      case Left(errors)     => Left(errors.toChain.toList.map(_.render))
      case Right(validated) =>
        val check    = new Check(schema, validated.document)
        val problems = check.variables(validated.operation, variables) ++ check.data(validated.operation, data)
        if (problems.isEmpty) Right(validated.operation.name) else Left(problems)
    }

  /** A selected field: its type, what is selected beneath it, the types of parent whose response carries it, and
    * whether a directive may leave it out.
    */
  final private case class Selected(
      tpe: TypeRef,
      selections: List[(String, Selection)],
      on: Set[String],
      skippable: Boolean
  )

  private val leaves: Map[String, Set[String]] = Map(
    "String"  -> Set("String"),
    "Boolean" -> Set("Boolean"),
    "Int"     -> Set("Int", "Long", "BigInt"),
    "Float"   -> Set("Double", "Float", "BigDecimal")
  )
}

final private class Check(schema: Schema, document: ExecutableDocument) {

  import Check.*

  def variables(operation: OperationDefinition, shape: Shape): List[String] =
    (operation.variables, shape) match {
      case (Nil, Shape.Leaf("Unit"))              => Nil
      case (Nil, Shape.Record(_, Nil))            => Nil
      case (Nil, other)                           =>
        List(s"the operation declares no variables, so its variables are Unit, not ${other.render}")
      case (declared, Shape.Record(name, fields)) =>
        val byName   = fields.toMap
        val missing  = declared.filterNot(variable => byName.contains(variable.name)).map { variable =>
          s"$name has no field for the variable '$$${variable.name}'"
        }
        val unknown  = fields.map(_._1).filterNot(field => declared.exists(_.name == field)).map { field =>
          s"$name has a field '$field', but the operation declares no variable '$$$field'"
        }
        val optional = declared.flatMap { variable =>
          byName.get(variable.name).collect {
            case Shape.Optional(_) if variable.tpe.isNonNull && variable.default.isEmpty =>
              s"'$$${variable.name}' must be given a value, so $name.${variable.name} cannot be an Option"
          }
        }
        missing ++ unknown ++ optional
      case (_, other)                             =>
        List(s"the operation declares variables, so they are written as a case class, not ${other.render}")
    }

  def data(operation: OperationDefinition, shape: Shape): List[String] = {
    val root = operation.kind match {
      case OperationKind.Query        => schema.query
      case OperationKind.Mutation     => schema.mutation
      case OperationKind.Subscription => schema.subscription
    }
    root match {
      case None       => List(s"the schema has no ${operation.kind.label} type")
      case Some(name) => record(name, operation.selections.map(name -> _), shape, operation.kind.label)
    }
  }

  /** `shape` against a value of GraphQL type `tpe` at `path`. */
  private def value(
      tpe: TypeRef,
      selections: List[(String, Selection)],
      conditional: Boolean,
      shape: Shape,
      path: String
  ): List[String] =
    (tpe, shape) match {
      case (TypeRef.NonNull(of), Shape.Optional(inner)) => required(of, selections, inner, path)
      case (TypeRef.NonNull(of), other) if !conditional => required(of, selections, other, path)
      case (TypeRef.NonNull(_), other)                  =>
        List(
          s"$path is only in the response for some types or conditions, so it is read as an Option, not ${other.render}"
        )
      case (nullable, Shape.Optional(inner))            => required(nullable, selections, inner, path)
      case (_, other)                                   =>
        List(s"$path is nullable in the schema, so it is read as an Option, not ${other.render}")
    }

  private def required(tpe: TypeRef, selections: List[(String, Selection)], shape: Shape, path: String): List[String] =
    (tpe, shape) match {
      case (TypeRef.NonNull(of), other)              => required(of, selections, other, path)
      case (TypeRef.ListOf(of), Shape.Many(element)) => value(of, selections, conditional = false, element, s"$path[]")
      case (TypeRef.ListOf(_), other)                =>
        List(s"$path is a list in the schema, so it is read as a collection, not ${other.render}")
      case (TypeRef.Named(name), other)              =>
        schema.types.get(name).map(_.kind) match {
          case Some(TypeKind.ObjectType | TypeKind.InterfaceType | TypeKind.UnionType) =>
            record(name, selections.map((_, selection) => name -> selection), other, path)
          case _                                                                       => leaf(name, other, path)
        }
    }

  private def leaf(name: String, shape: Shape, path: String): List[String] =
    shape match {
      case Shape.Leaf(scala) =>
        leaves.get(name).filterNot(_.contains(scala)).toList.map { allowed =>
          s"$path is a $name in the schema, so it is read as ${allowed.toList.sorted.mkString(" or ")}, not $scala"
        }
      case other             =>
        List(s"$path is the scalar or enum $name in the schema, so it cannot be read as ${other.render}")
    }

  /** A case class against the fields selected on `parent`. Each selection comes with the type it was written on. */
  private def record(parent: String, selections: List[(String, Selection)], shape: Shape, path: String): List[String] =
    shape match {
      case Shape.Record(name, fields) =>
        val every    = possible(parent)
        val selected = collect(selections, every, skippable = false)
        val keys     = selected.map(_._1).distinct
        val byKey    = keys.map(key => key -> selected.collect { case (`key`, found) => found }).toMap
        val unread   = keys.filterNot(key => key == "__typename" || fields.exists(_._1 == key)).map { key =>
          s"$path.$key is selected, but $name never reads it"
        }
        val read     = fields.flatMap { case (field, fieldShape) =>
          byKey.get(field) match {
            case None        => List(s"$name reads '$field', which the operation does not select at $path")
            case Some(found) =>
              // one response key selected in several fragments is one field, present wherever any of them applies
              val conditional = found.forall(_.skippable) || !every.subsetOf(found.flatMap(_.on).toSet)
              value(found.head.tpe, found.flatMap(_.selections), conditional, fieldShape, s"$path.$field")
          }
        }
        unread ++ read
      case other                      =>
        List(s"$path is the object type $parent in the schema, so it is read as a case class, not ${other.render}")
    }

  /** The response keys selected, with fragments expanded. `on` is the types of the parent the selections apply to. */
  private def collect(
      selections: List[(String, Selection)],
      on: Set[String],
      skippable: Boolean
  ): List[(String, Selected)] =
    selections.flatMap { case (written, selection) =>
      val maybe =
        skippable || selection.directives.exists(directive => directive.name == "skip" || directive.name == "include")
      selection match {
        case field: FieldSelection if field.name == "__typename" =>
          List(field.responseKey -> Selected(TypeRef.NonNull(TypeRef.Named("String")), Nil, on, maybe))
        case field: FieldSelection                               =>
          schema.types.get(written).flatMap(_.fields.find(_.name == field.name)).toList.map { definition =>
            field.responseKey -> Selected(definition.tpe, field.selections.map(definition.tpe.named -> _), on, maybe)
          }
        case inline: InlineFragment                              =>
          val condition = inline.typeCondition.getOrElse(written)
          collect(inline.selections.map(condition -> _), on & possible(condition), maybe)
        case spread: FragmentSpread                              =>
          document.fragment(spread.name).toList.flatMap { fragment =>
            collect(fragment.selections.map(fragment.typeCondition -> _), on & possible(fragment.typeCondition), maybe)
          }
      }
    }

  /** The object types a value of the named type can be. */
  private def possible(name: String): Set[String] =
    schema.types.get(name) match {
      case Some(tpe) if tpe.kind == TypeKind.UnionType     => tpe.unionMembers.toSet
      case Some(tpe) if tpe.kind == TypeKind.InterfaceType =>
        schema.types.values.filter(_.interfaces.contains(name)).map(_.name).toSet
      case _                                               => Set(name)
    }
}
