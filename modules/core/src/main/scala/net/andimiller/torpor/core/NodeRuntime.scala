package net.andimiller.torpor.core

import cats.data.{Chain, NonEmptyList}
import cats.effect.Concurrent
import cats.syntax.all._
import io.circe.Json

/** What one visit to a node produced: a result, or a suspension handle. */
enum NodeOutcome {
  case Completed(result: NodeResult)
  case Suspended(handle: Json)
}

/** Node execution: one batch call, keys deduped, errors as values. Shared by the request executor and the durable
  * runner so both behave identically; the durable one is the only one that can see `Suspended`.
  */
object NodeRuntime {

  def run[F[_]: Concurrent](
      plan: Plan,
      node: PlanNode,
      parent: Option[NodeResult],
      resolver: Resolver[F],
      rootKey: Option[String]
  ): F[NodeOutcome] =
    // Boundary: `defer` moves construction of the resolver's F inside the effect, so a synchronous throw (the DSL
    // throws when arguments do not decode) becomes a field error instead of escaping into hedgehogs' fail-fast
    // segment, which would cancel in-flight fibers and lose the snapshot.
    try
      execute(plan, node, parent, resolver, rootKey)
        .handleErrorWith(t => Concurrent[F].pure(NodeOutcome.Completed(upstreamFailure(pathOf(rootKey), t))))
    catch {
      // A synchronous throw happens while the resolver's effect is being built; catching it here keeps hedgehogs' fair
      // catch clause honest (it must never see a raised node).
      case t: Throwable => Concurrent[F].pure(NodeOutcome.Completed(upstreamFailure(pathOf(rootKey), t)))
    }

  private def execute[F[_]: Concurrent](
      plan: Plan,
      node: PlanNode,
      parent: Option[NodeResult],
      resolver: Resolver[F],
      rootKey: Option[String]
  ): F[NodeOutcome] =
    resolver match {
      case Resolver.Suspending(start, _) =>
        start(node.args).attempt.map {
          case Right(Step.Complete(resolved)) => NodeOutcome.Completed(completed(pathOf(rootKey), resolved))
          case Right(Step.Suspend(handle))    => NodeOutcome.Suspended(handle)
          case Left(t)                        => NodeOutcome.Completed(upstreamFailure(pathOf(rootKey), t))
        }

      case Resolver.Root(run) =>
        run(node.args).attempt.map {
          case Right(resolved) => NodeOutcome.Completed(completed(pathOf(rootKey), resolved))
          case Left(t)         => NodeOutcome.Completed(upstreamFailure(pathOf(rootKey), t))
        }

      case Resolver.Batch(key, batch) =>
        val positions = parent.toList.flatMap(result => inputPositions(plan, node, result))

        val extracted: List[(List[PathSegment], Either[FieldError, Option[Any]])] =
          positions.map { case (path, parent) =>
            if (parent.json.isNull) (path, Right(None))
            else (path, key(parent).left.map(message => FieldError(message, path, FieldError.InvalidParentValue)))
          }

        val keys                                        = extracted.collect { case (_, Right(Some(k))) => k }.distinct
        val call: F[Either[String, Map[Any, Resolved]]] = NonEmptyList.fromList(keys) match {
          case None      => Concurrent[F].pure(Right(Map.empty[Any, Resolved]))
          case Some(nel) => batch(nel).attempt.map(_.left.map(t => Option(t.getMessage).getOrElse(t.toString)))
        }

        call.map {
          case Left(message) =>
            NodeOutcome.Completed(
              NodeResult(
                extracted.map { case (path, _) => path -> Json.Null },
                Chain.fromSeq(extracted.map { case (path, _) =>
                  FieldError(message, path, FieldError.UpstreamUnavailable)
                })
              )
            )

          case Right(results) =>
            val values: List[(List[PathSegment], Option[Resolved], Chain[FieldError])] = extracted.map {
              case (path, Left(error))    => (path, None, Chain.one(error))
              case (path, Right(None))    => (path, None, Chain.empty[FieldError])
              case (path, Right(Some(k))) =>
                results.get(k) match {
                  case Some(resolved) => (path, Some(resolved), Chain.empty[FieldError])
                  case None           =>
                    (path, None, Chain.one(FieldError(s"no result for key '$k'", path, FieldError.MissingKey)))
                }
            }
            NodeOutcome.Completed(
              NodeResult(
                values.map { case (path, resolved, _) => path -> resolved.fold(Json.Null)(_.json) },
                values.foldLeft(Chain.empty[FieldError]) { case (acc, (_, _, errs)) => acc ++ errs },
                values.collect { case (path, Some(resolved), _) => path -> resolved.value }.toMap
              )
            )
        }
    }

  /** Where this node's values sit inside its parent's output. Deterministic, so execution and assembly agree. */
  def inputPositions(plan: Plan, node: PlanNode, parent: NodeResult): List[(List[PathSegment], Parent)] =
    node.dependencies.headOption.flatMap(plan.node) match {
      case None             => Nil
      case Some(parentNode) =>
        parent.values.flatMap { case (path, value) =>
          val produced = parent.produced.get(path).map(Produced(parentNode.field, _))
          childPositions(value, produced, parentNode.selectionSet, path, node.id)
        }
    }

  /** Positions of a specific child node's parent values, in response order. Inline fields only contribute a path
    * prefix; only the matching `NodeField` yields values, otherwise sibling scalars would be mistaken for parent
    * values.
    *
    * The Scala value is walked next to the JSON, through `Option`s and collections, so a parent that its resolver
    * produced directly reaches the child as that value. Inside an inline field there is no Scala value to walk, so a
    * child there gets only the JSON.
    */
  def childPositions(
      value: Json,
      produced: Option[Produced],
      selectionSet: SelectionSet,
      path: List[PathSegment],
      childId: NodeId
  ): List[(List[PathSegment], Parent)] =
    value.asArray match {
      case Some(elements)       =>
        val producedElements = produced.flatMap { p =>
          elementsOf(p.value).filter(_.size == elements.size).map(_.map(Produced(p.by, _)))
        }
        elements.toList.zipWithIndex.flatMap { case (element, index) =>
          childPositions(
            element,
            producedElements.map(_(index)),
            selectionSet,
            path :+ PathSegment.Index(index),
            childId
          )
        }
      case None if value.isNull => Nil
      case None                 =>
        selectionSet.selections.flatMap {
          case InlineField(responseKey, field, selections)    =>
            childPositions(member(value, field), None, selections, path :+ PathSegment.Field(responseKey), childId)
          case NodeField(responseKey, _, id) if id == childId =>
            // The key comes from the enclosing parent value, never from the parent's data for this field.
            List(
              (path :+ PathSegment.Field(responseKey)) -> Parent(
                value,
                produced.map(p => p.copy(value = present(p.value)))
              )
            )
          case NodeField(_, _, _)                             => Nil
        }
    }

  private def elementsOf(value: Any): Option[Vector[Any]] = value match {
    case Some(inner)         => elementsOf(inner)
    case values: Iterable[?] => Some(values.toVector)
    case _                   => None
  }

  private def present(value: Any): Any = value match {
    case Some(inner) => present(inner)
    case other       => other
  }

  private def completed(path: List[PathSegment], resolved: Resolved): NodeResult =
    NodeResult(List(path -> resolved.json), Chain.empty, Map(path -> resolved.value))

  /** A root node's response path, supplied from `Plan.roots` so errors carry an absolute path. */
  private def pathOf(rootKey: Option[String]): List[PathSegment]                 =
    rootKey.map(key => List(PathSegment.Field(key))).getOrElse(Nil)

  private def upstreamFailure(path: List[PathSegment], t: Throwable): NodeResult =
    NodeResult(
      List(path -> Json.Null),
      Chain.one(FieldError(Option(t.getMessage).getOrElse(t.toString), path, FieldError.UpstreamUnavailable))
    )

  def member(value: Json, field: String): Json =
    value.asObject.flatMap(_(field)).getOrElse(Json.Null)
}
