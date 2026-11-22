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
  *   - a union or interface may instead be a sealed trait with one case class per type the value can be, named as the
  *     type is, each read against the fields selected for that type; the document must select `__typename` there, which
  *     is what the trait's decoder tells the members apart by;
  *   - a nullable field is an `Option`; so is a field selected inside a fragment that only some types match, or behind
  *     `@skip` or `@include`, because the response may not carry it;
  *   - a list is a collection;
  *   - `String`, `Boolean`, `Int` and `Float` are read as the Scala types that hold them. An enum may be read as a
  *     union of its values as string literals, such as `"DOG" | "CAT"`, or as `String`. Every other scalar may be read
  *     as any type that is not a case class or a collection: its decoder decides.
  *
  * And for the type the variables are written as: a case class with one field per variable the operation declares,
  * where a variable that must be given a value is not an `Option`, and each field fits the variable's type the way a
  * value given by name does. An operation with no variables takes `Unit`.
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

  /** The concrete type of each object on the path from the root to a value. A selection in a fragment applies to some
    * worlds and not others, which is what decides whether a field is always in the response.
    */
  type World = List[String]

  /** A selection as written: on which type, and in which worlds it applies. */
  final case class Entry(written: String, selection: Selection, worlds: Set[World])

  /** What is selected on an object: its type, the selections, and every world a value of it may be in. */
  final case class Selections(parent: String, entries: List[Entry], worlds: Set[World])

  /** A selected field: where the schema declares it, its type, what is selected beneath it, the worlds it is in, and
    * whether a directive may leave it out.
    */
  final private case class Selected(
      owner: String,
      name: String,
      tpe: TypeRef,
      beneath: List[Entry],
      on: Set[World],
      skippable: Boolean
  )

  /** One response key of an object: the field the schema declares it as, its type, what is selected beneath it, the
    * worlds it is selected in, and whether the response may leave it out.
    */
  final case class Field(
      key: String,
      owner: String,
      name: String,
      tpe: TypeRef,
      beneath: Selections,
      on: Set[World],
      conditional: Boolean
  )

  /** A type as SDL writes it, such as `[ID!]!`. */
  def render(tpe: TypeRef): String = tpe match {
    case TypeRef.Named(name) => name
    case TypeRef.ListOf(of)  => s"[${render(of)}]"
    case TypeRef.NonNull(of) => s"${render(of)}!"
  }

  /** The inverse of [[render]]. */
  def parse(text: String): TypeRef =
    if (text.endsWith("!")) TypeRef.NonNull(parse(text.dropRight(1)))
    else if (text.startsWith("[")) TypeRef.ListOf(parse(text.drop(1).dropRight(1)))
    else TypeRef.Named(text)

  /** Scala types that can never be a whole input object. */
  private val primitives: Set[String] =
    Set("String", "Boolean", "Int", "Long", "Double", "Float", "BigInt", "BigDecimal", "Unit")

  private val leaves: Map[String, Set[String]] = Map(
    "String"  -> Set("String"),
    "Boolean" -> Set("Boolean"),
    "Int"     -> Set("Int", "Long", "BigInt"),
    "Float"   -> Set("Double", "Float", "BigDecimal")
  )

}

final private[client] class Check(schema: Schema, document: ExecutableDocument) {

  import Check.*

  def variables(operation: OperationDefinition, shape: Shape): List[String] =
    (operation.variables, shape) match {
      case (Nil, Shape.Leaf("Unit"))              => Nil
      case (Nil, Shape.Record(_, Nil))            => Nil
      case (Nil, other)                           =>
        List(s"the operation declares no variables, so its variables are Unit, not ${other.render}")
      case (declared, Shape.Record(name, fields)) =>
        val byName  = fields.toMap
        val missing = declared.filterNot(variable => byName.contains(variable.name)).map { variable =>
          s"$name has no field for the variable '$$${variable.name}'"
        }
        val unknown = fields.map(_._1).filterNot(field => declared.exists(_.name == field)).map { field =>
          s"$name has a field '$field', but the operation declares no variable '$$$field'"
        }
        val fit     = declared.flatMap { variable =>
          byName.get(variable.name).toList.flatMap {
            case Shape.Optional(_) if variable.tpe.isNonNull && variable.default.isEmpty =>
              List(s"'$$${variable.name}' must be given a value, so $name.${variable.name} cannot be an Option")
            case Shape.Optional(inner)                                                   =>
              inputValue(variable.tpe, inner, s"$name.${variable.name}")
            case other                                                                   =>
              inputValue(variable.tpe, other, s"$name.${variable.name}")
          }
        }
        missing ++ unknown ++ fit
      case (_, other)                             =>
        List(s"the operation declares variables, so they are written as a case class, not ${other.render}")
    }

  def data(operation: OperationDefinition, shape: Shape): List[String] =
    root(operation) match {
      case None       => List(s"the schema has no ${operation.kind.label} type")
      case Some(from) => record(from, shape, operation.kind.label)
    }

  /** `shape` against a value of GraphQL type `tpe` at `path`. */
  private def value(tpe: TypeRef, beneath: Selections, conditional: Boolean, shape: Shape, path: String): List[String] =
    (tpe, shape) match {
      case (TypeRef.NonNull(of), Shape.Optional(inner)) => required(of, beneath, inner, path)
      case (TypeRef.NonNull(of), other) if !conditional => required(of, beneath, other, path)
      case (TypeRef.NonNull(_), other)                  =>
        List(
          s"$path is only in the response for some types or conditions, so it is read as an Option, not ${other.render}"
        )
      case (nullable, Shape.Optional(inner))            => required(nullable, beneath, inner, path)
      case (_, other)                                   =>
        List(s"$path is nullable in the schema, so it is read as an Option, not ${other.render}")
    }

  private def required(tpe: TypeRef, beneath: Selections, shape: Shape, path: String): List[String] =
    (tpe, shape) match {
      case (TypeRef.NonNull(of), other)              => required(of, beneath, other, path)
      case (TypeRef.ListOf(of), Shape.Many(element)) => value(of, beneath, conditional = false, element, s"$path[]")
      case (TypeRef.ListOf(_), other)                =>
        List(s"$path is a list in the schema, so it is read as a collection, not ${other.render}")
      case (TypeRef.Named(name), other)              =>
        if (isObject(name)) record(beneath, other, path) else leaf(name, other, path)
    }

  private def leaf(name: String, shape: Shape, path: String, verb: String = "read"): List[String] =
    shape match {
      case Shape.Trusted(_)         => Nil
      case Shape.Leaf(scala)        =>
        leaves.get(name).filterNot(_.contains(scala)).toList.map { allowed =>
          s"$path is a $name in the schema, so it is $verb as ${allowed.toList.sorted.mkString(" or ")}, not $scala"
        }
      // a literal is a string, unless the type is an enum: then each literal is one of its values
      case Shape.Literals(literals) =>
        schema.types.get(name).filter(_.kind == TypeKind.EnumType) match {
          case None             => leaf(name, Shape.Leaf("String"), path, verb)
          case Some(definition) =>
            val values = definition.enumValues.map(_.name)
            literals.filterNot(values.contains).map { literal =>
              s"$path is the enum $name in the schema, but \"$literal\" is not one of its values: ${values.mkString(", ")}"
            }
        }
      case other                    =>
        List(s"$path is the scalar or enum $name in the schema, so it cannot be $verb as ${other.render}")
    }

  /** A value given for an input of type `tpe`, or the absence of one. An input that must be given a value is not an
    * `Option`, nor `null`; a list is a collection; an input object is a record with no field the schema lacks and every
    * field the schema requires.
    */
  def input(tpe: TypeRef, hasDefault: Boolean, value: Option[Shape], path: String): List[String] =
    (tpe, value) match {
      case (TypeRef.NonNull(_), None) if !hasDefault                    => List(s"$path must be given a value")
      case (_, None)                                                    => Nil
      case (TypeRef.NonNull(_), Some(Shape.Null))                       =>
        List(s"$path must be given a value, so it cannot be null")
      case (_, Some(Shape.Null))                                        => Nil
      case (TypeRef.NonNull(_), Some(Shape.Optional(_))) if !hasDefault =>
        List(s"$path must be given a value, so it cannot be an Option")
      case (_, Some(Shape.Optional(inner)))                             => inputValue(tpe, inner, path)
      case (_, Some(shape))                                             => inputValue(tpe, shape, path)
    }

  private def inputValue(tpe: TypeRef, shape: Shape, path: String): List[String] =
    (tpe, shape) match {
      case (_, Shape.Trusted(_))                     => Nil
      case (TypeRef.NonNull(of), other)              => inputValue(of, other, path)
      case (TypeRef.ListOf(of), Shape.Many(element)) => input(of, hasDefault = false, Some(element), s"$path[]")
      case (TypeRef.ListOf(_), other)                =>
        List(s"$path is a list in the schema, so it is written as a collection, not ${other.render}")
      case (TypeRef.Named(name), other)              =>
        schema.types.get(name) match {
          case Some(definition) if definition.kind == TypeKind.InputObjectType =>
            other match {
              case Shape.Record(_, fields)                 =>
                val declared = definition.fields.map(_.name).toSet
                val unknown  = fields.map(_._1).filterNot(declared).map { field =>
                  s"$path has a field '$field', which the input type $name does not have"
                }
                unknown ++ definition.fields.flatMap { field =>
                  input(field.tpe, field.defaultValue.isDefined, fields.toMap.get(field.name), s"$path.${field.name}")
                }
              // a type of the caller's own stands for the whole input object: its encoder decides what is sent
              case Shape.Leaf(scala) if !primitives(scala) => Nil
              case notRecord                               =>
                List(
                  s"$path is the input type $name in the schema, so it is written with input(…), not ${notRecord.render}"
                )
            }
          case _                                                               => leaf(name, other, path, verb = "written")
        }
    }

  /** The type an operation's response starts from, with the operation's selections on it. */
  def root(operation: OperationDefinition): Option[Selections] = {
    val name = operation.kind match {
      case OperationKind.Query        => schema.query
      case OperationKind.Mutation     => schema.mutation
      case OperationKind.Subscription => schema.subscription
    }
    name.map { root =>
      val world = Set(List(root))
      Selections(root, operation.selections.map(Entry(root, _, world)), world)
    }
  }

  /** The response keys selected, in the order the document first selects them, or the keys that cannot be one field.
    *
    * One response key selected in several fragments is one field, present wherever any of them applies. It has to have
    * one type for that, which the validator does not check across fragments.
    */
  def fields(selections: Selections, path: String): Either[List[String], List[Field]] = {
    val selected = collect(selections.entries, skippable = false)
    val keys     = selected.map(_._1).distinct
    val found    = keys.map(key => key -> selected.collect { case (`key`, field) => field })

    val conflicts = found.flatMap { case (key, occurrences) =>
      occurrences.map(_.tpe).distinct match {
        case _ :: Nil | Nil => Nil
        case _              =>
          val each = occurrences.map(o => s"${render(o.tpe)} on ${o.owner}").distinct
          List(s"$path.$key is ${each.mkString(" and ")}, so it cannot be read as one field")
      }
    }

    if (conflicts.nonEmpty) Left(conflicts)
    else
      Right(found.map { case (key, occurrences) =>
        val first = occurrences.head
        val on    = occurrences.flatMap(_.on).toSet
        Field(
          key = key,
          owner = first.owner,
          name = first.name,
          tpe = first.tpe,
          beneath = Selections(first.tpe.named, occurrences.flatMap(_.beneath), within(on, first.tpe.named)),
          on = on,
          conditional = occurrences.forall(_.skippable) || !selections.worlds.subsetOf(on)
        )
      })
  }

  /** The view of a union or interface value as one of the types it can be: the selections that apply in the worlds
    * where it is that type.
    */
  def view(selections: Selections, member: String): Selections =
    Selections(
      member,
      selections.entries.map(entry => entry.copy(worlds = entry.worlds.filter(_.last == member))),
      selections.worlds.filter(_.last == member)
    )

  /** The object types a value of the named type can be, in a stable order. */
  def possibleTypes(name: String): List[String] = possible(name).toList.sorted

  /** A case class against the fields selected on an object, or a sealed trait against each type the object can be. */
  private def record(selections: Selections, shape: Shape, path: String): List[String] =
    shape match {
      case Shape.Sealed(name, members) =>
        fields(selections, path) match {
          case Left(conflicts) => conflicts
          case Right(selected) =>
            val universe = possibleTypes(selections.parent)
            val names    = members.map(_.render)
            val tagged   =
              if (selected.exists(field => field.name == "__typename" && !field.conditional)) Nil
              else List(s"$path is read as $name, a sealed trait, so the operation must select __typename at $path")
            val unknown  = names.filterNot(universe.contains).map { member =>
              s"$name has a member $member, which is not a type that $path can be: ${universe.mkString(", ")}"
            }
            val missing  = universe.filterNot(names.contains).map { member =>
              s"$name has no member for $member, which $path can be"
            }
            val each     = members.flatMap {
              case member @ Shape.Record(memberName, _) if universe.contains(memberName) =>
                record(view(selections, memberName), member, s"$path as $memberName")
              case member if universe.contains(member.render)                            =>
                List(s"$name has a member ${member.render}, which is not a case class")
              case _                                                                     => Nil
            }
            tagged ++ unknown ++ missing ++ each
        }
      case Shape.Record(name, read)    =>
        fields(selections, path) match {
          case Left(conflicts) => conflicts
          case Right(selected) =>
            val unread = selected.filterNot(field => field.key == "__typename" || read.exists(_._1 == field.key)).map {
              field => s"$path.${field.key} is selected, but $name never reads it"
            }
            val fit    = read.flatMap { case (key, fieldShape) =>
              selected.find(_.key == key) match {
                case None        => List(s"$name reads '$key', which the operation does not select at $path")
                case Some(field) => value(field.tpe, field.beneath, field.conditional, fieldShape, s"$path.$key")
              }
            }
            unread ++ fit
        }
      case other                       =>
        List(
          s"$path is the object type ${selections.parent} in the schema, so it is read as a case class, not ${other.render}"
        )
    }

  /** The response keys selected, with fragments expanded. A selection that applies in no world selects nothing. */
  private def collect(entries: List[Entry], skippable: Boolean): List[(String, Selected)] =
    entries.filter(_.worlds.nonEmpty).flatMap { case Entry(written, selection, worlds) =>
      val maybe =
        skippable || selection.directives.exists(directive => directive.name == "skip" || directive.name == "include")
      selection match {
        case field: FieldSelection if field.name == "__typename" =>
          val tpe = TypeRef.NonNull(TypeRef.Named("String"))
          List(field.responseKey -> Selected(written, "__typename", tpe, Nil, worlds, maybe))
        case field: FieldSelection                               =>
          schema.types.get(written).flatMap(_.fields.find(_.name == field.name)).toList.map { definition =>
            val beneath = within(worlds, definition.tpe.named)
            field.responseKey -> Selected(
              written,
              field.name,
              definition.tpe,
              field.selections.map(Entry(definition.tpe.named, _, beneath)),
              worlds,
              maybe
            )
          }
        case inline: InlineFragment                              =>
          val condition = inline.typeCondition.getOrElse(written)
          collect(inline.selections.map(Entry(condition, _, matching(worlds, condition))), maybe)
        case spread: FragmentSpread                              =>
          document.fragment(spread.name).toList.flatMap { fragment =>
            val condition = fragment.typeCondition
            collect(fragment.selections.map(Entry(condition, _, matching(worlds, condition))), maybe)
          }
      }
    }

  /** The worlds a fragment on `condition` applies to. */
  private def matching(worlds: Set[World], condition: String): Set[World] = {
    val members = possible(condition)
    worlds.filter(world => members(world.last))
  }

  /** The worlds beneath a field of the named type: each world, extended with each type the value can be. */
  private def within(worlds: Set[World], name: String): Set[World] =
    if (isObject(name)) worlds.flatMap(world => possible(name).map(world :+ _)) else Set.empty

  private def isObject(name: String): Boolean =
    schema.types.get(name).exists { tpe =>
      tpe.kind == TypeKind.ObjectType || tpe.kind == TypeKind.InterfaceType || tpe.kind == TypeKind.UnionType
    }

  /** The object types a value of the named type can be. */
  private def possible(name: String): Set[String] =
    schema.types.get(name) match {
      case Some(tpe) if tpe.kind == TypeKind.UnionType     => tpe.unionMembers.toSet
      // an interface may implement this one too, but a value is always an object
      case Some(tpe) if tpe.kind == TypeKind.InterfaceType =>
        schema.types.values.filter(t => t.kind == TypeKind.ObjectType && t.interfaces.contains(name)).map(_.name).toSet
      case _                                               => Set(name)
    }
}
