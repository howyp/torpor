package net.andimiller.torpor.source

import cats.{Applicative, Functor}
import cats.data.NonEmptyList
import cats.syntax.all._
import io.circe.{Decoder, Encoder, Json, parser}
import net.andimiller.torpor.core.{Arguments, FieldCoordinate, InvalidInputException, Resolver, Step}

/** A source: an SDL fragment plus the resolvers that serve it. */
final case class Source[F[_]](id: String, sdl: String, resolvers: Resolvers[F])

/** Registered resolvers, keyed by the field coordinate the merged schema gives them. */
final case class Resolvers[F[_]](entries: List[(FieldCoordinate, Resolver[F])]) {
  def ++(other: Resolvers[F]): Resolvers[F] = Resolvers(entries ++ other.entries)
}

object Resolvers {
  def apply[F[_]](entries: (FieldCoordinate, Resolver[F])*): Resolvers[F] = Resolvers(entries.toList)
  def empty[F[_]]: Resolvers[F]                                           = Resolvers(Nil)
}

// wiring code imports `source.*`; these are what it declares and reads
export net.andimiller.torpor.core.{Arguments, Scalar}

/** Resolvers take coordinates from the schema macro (`schema"""…"""`), whose types carry each field's parent, name,
  * arguments and GraphQL return type.
  */
object resolver {

  /** A root field. The lambda's `args` is typed from the SDL's arguments: `resolver.root[IO](sdl.Query.character) {
    * args => args.id … }`; `.args[A]` decodes them as `A` instead.
    */
  def root[F[_]]: RootStart[F] = new RootStart[F]

  final class RootStart[F[_]] {
    def apply[T <: String, N <: String, A, R <: String](
        coordinate: FieldCoordinate.Of[T, N, A, R]
    ): Root[F, A] =
      new Root(coordinate)
  }

  final class Root[F[_], A](coordinate: FieldCoordinate) {
    inline def apply[B](run: A => F[B])(using
        functor: Functor[F],
        encoder: Encoder[B]
    ): (FieldCoordinate, Resolver[F]) =
      rootEntry[F, A, B](coordinate, run)(using functor, Arguments.decoder[A], encoder)

    /** Decode the arguments as `A2` instead, such as a case class with domain types the SDL cannot express. */
    def args[A2]: Root.Decoding[F, A2] = new Root.Decoding(coordinate)
  }

  object Root {
    final class Decoding[F[_], A](coordinate: FieldCoordinate) {
      def apply[B](run: A => F[B])(using
          Functor[F],
          Decoder[A],
          Encoder[B]
      ): (FieldCoordinate, Resolver[F]) =
        rootEntry[F, A, B](coordinate, run)
    }
  }

  /** A root field that may park the run. The handle is encoded into the snapshot and decoded back on resume; the
    * webhook payload stays raw JSON, because it arrives from outside.
    */
  def suspending[F[_]]: SuspendingStart[F] = new SuspendingStart[F]

  final class SuspendingStart[F[_]] {
    def apply[T <: String, N <: String, A, R <: String](
        coordinate: FieldCoordinate.Of[T, N, A, R]
    ): Suspending[F, A] =
      new Suspending(coordinate)
  }

  final class Suspending[F[_], A](coordinate: FieldCoordinate) {
    inline def apply[H, B](start: A => F[Step[B, H]])(resume: (H, Json) => F[Step[B, H]])(using
        functor: Functor[F],
        encodeHandle: Encoder[H],
        decodeHandle: Decoder[H],
        encoder: Encoder[B]
    ): (FieldCoordinate, Resolver[F]) =
      suspendingEntry[F, A, H, B](coordinate, start, resume)(using
        functor,
        Arguments.decoder[A],
        encodeHandle,
        decodeHandle,
        encoder
      )

  }

  def field[F[_]]: FieldStart[F] = new FieldStart[F]

  final class FieldStart[F[_]] {
    def apply[T <: String, N <: String, A, R <: String](coordinate: FieldCoordinate.Of[T, N, A, R]): Field[F] =
      new Field(coordinate)
  }

  final class Field[F[_]](coordinate: FieldCoordinate) {
    def key[P, K](extract: P => K): KeyedField[F, P, K] = new KeyedField(coordinate, extract)
  }

  /** A keyed field both consumes its parent and produces its own values, which are the next level's parents.
    *
    * The batch signature is `NonEmptyList[K] => F[Map[K, V]]`: there is deliberately no single-key overload, because
    * its absence is what makes N+1 impossible to write by accident.
    */
  final class KeyedField[F[_], P, K](
      coordinate: FieldCoordinate,
      extract: P => K
  ) {
    def batch[V](run: NonEmptyList[K] => F[Map[K, V]])(using
        Applicative[F],
        Decoder[P],
        Encoder[K],
        Decoder[K],
        Encoder[V]
    ): (FieldCoordinate, Resolver[F]) =
      keyedEntry(coordinate, extract, run)
  }

  /** A keyed field whose key may be absent (a nullable foreign key). Absent keys are never sent to `batch`; they
    * resolve to `null`, which is what "no value" means for a nullable field, and is not an error.
    */
  def optionalField[F[_]]: OptionalFieldStart[F] = new OptionalFieldStart[F]

  final class OptionalFieldStart[F[_]] {
    def apply[T <: String, N <: String, A, R <: String](
        coordinate: FieldCoordinate.Of[T, N, A, R]
    ): OptionalField[F] =
      new OptionalField(coordinate)
  }

  final class OptionalField[F[_]](coordinate: FieldCoordinate) {
    def key[P, K](extract: P => Option[K]): OptionalKeyedField[F, P, K] =
      new OptionalKeyedField(coordinate, extract)
  }

  final class OptionalKeyedField[F[_], P, K](
      coordinate: FieldCoordinate,
      extract: P => Option[K]
  ) {
    def batch[V](run: NonEmptyList[K] => F[Map[K, V]])(using
        Applicative[F],
        Decoder[P],
        Encoder[K],
        Decoder[K],
        Encoder[V]
    ): (FieldCoordinate, Resolver[F]) =
      optionalKeyedEntry(coordinate, extract, run)
  }

  // the erased resolvers the executor runs

  /** Not a valid JSON key encoding, so it cannot collide with a real key: encoded keys start with a JSON token. */
  private val AbsentKey = "\u0000absent"

  private def argumentsJson(arguments: List[net.andimiller.torpor.core.Argument]): Json =
    Json.obj(arguments.map(a => a.name -> a.value.toJson): _*)

  private def rootEntry[F[_]: Functor, A: Decoder, B: Encoder](
      coordinate: FieldCoordinate,
      run: A => F[B]
  ): (FieldCoordinate, Resolver[F])                                                     =
    coordinate -> Resolver.Root { arguments =>
      Decoder[A].decodeJson(argumentsJson(arguments)) match {
        case Left(error)    => throw new InvalidInputException(s"cannot decode arguments for ${coordinate.render}: $error")
        case Right(decoded) => run(decoded).map(Encoder[B].apply)
      }
    }

  private def suspendingEntry[F[_]: Functor, A: Decoder, H: Encoder: Decoder, B: Encoder](
      coordinate: FieldCoordinate,
      start: A => F[Step[B, H]],
      resume: (H, Json) => F[Step[B, H]]
  ): (FieldCoordinate, Resolver[F]) = {
    def encoded(step: F[Step[B, H]]): F[Step[Json, Json]] = step.map {
      case Step.Complete(value) => Step.Complete(Encoder[B].apply(value))
      case Step.Suspend(handle) => Step.Suspend(Encoder[H].apply(handle))
    }

    coordinate -> Resolver.Suspending(
      start = arguments =>
        Decoder[A].decodeJson(argumentsJson(arguments)) match {
          case Left(error)    =>
            throw new InvalidInputException(s"cannot decode arguments for ${coordinate.render}: $error")
          case Right(decoded) => encoded(start(decoded))
        },
      resume = (handle, payload) =>
        parser.parse(handle.noSpaces).flatMap(Decoder[H].decodeJson) match {
          case Left(error)    => throw new InvalidInputException(s"cannot decode handle for ${coordinate.render}: $error")
          case Right(decoded) => encoded(resume(decoded, payload))
        }
    )
  }

  private def keyedEntry[F[_]: Applicative, P: Decoder, K: Encoder: Decoder, V: Encoder](
      coordinate: FieldCoordinate,
      extract: P => K,
      run: NonEmptyList[K] => F[Map[K, V]]
  ): (FieldCoordinate, Resolver[F]) =
    coordinate -> Resolver.Batch(
      key = value =>
        Decoder[P].decodeJson(value) match {
          case Left(error) => Left(s"cannot decode parent value for ${coordinate.render}: $error")
          case Right(p)    => Right(keyString(extract(p)))
        },
      batch = keys =>
        run(keys.map(decodeKey[K](coordinate, _))).map(_.map { case (k, v) => keyString(k) -> Encoder[V].apply(v) })
    )

  private def optionalKeyedEntry[F[_]: Applicative, P: Decoder, K: Encoder: Decoder, V: Encoder](
      coordinate: FieldCoordinate,
      extract: P => Option[K],
      run: NonEmptyList[K] => F[Map[K, V]]
  ): (FieldCoordinate, Resolver[F]) =
    coordinate -> Resolver.Batch(
      key = value =>
        Decoder[P].decodeJson(value) match {
          case Left(error) => Left(s"cannot decode parent value for ${coordinate.render}: $error")
          case Right(p)    => Right(extract(p).fold(AbsentKey)(keyString(_)))
        },
      batch = keys => {
        val present = keys.toList.filterNot(_ == AbsentKey).distinct
        NonEmptyList.fromList(present) match {
          case None      => Applicative[F].pure(Map(AbsentKey -> Json.Null))
          case Some(nel) =>
            run(nel.map(decodeKey[K](coordinate, _)))
              .map(results =>
                results.map { case (k, v) => keyString(k) -> Encoder[V].apply(v) } + (AbsentKey -> Json.Null)
              )
        }
      }
    )

  private def keyString[K: Encoder](key: K): String = Encoder[K].apply(key).noSpaces

  private def decodeKey[K: Decoder](coordinate: FieldCoordinate, encoded: String): K =
    parser
      .parse(encoded)
      .toOption
      .flatMap(Decoder[K].decodeJson(_).toOption)
      .getOrElse(throw new InvalidInputException(s"cannot decode batch key '$encoded' for ${coordinate.render}"))
}
