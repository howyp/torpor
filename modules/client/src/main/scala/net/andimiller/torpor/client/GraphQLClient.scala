package net.andimiller.torpor.client

import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.{Decoder, Encoder, Json, JsonObject}
import org.http4s.{Method, Request, Uri}
import org.http4s.circe.*
import org.http4s.client.Client

/** One error from a GraphQL response: its message, where in the response it happened, and its code if it has one. */
final case class GraphQLError(message: String, path: List[String], code: Option[String])

object GraphQLError {

  given Decoder[GraphQLError] = Decoder.instance { cursor =>
    for {
      message <- cursor.get[String]("message")
      path    <- cursor.get[Option[List[Json]]]("path")
      code    <- cursor.downField("extensions").get[Option[String]]("code")
    } yield GraphQLError(
      message,
      // a path mixes field names and list indices
      path.toList.flatten.map(segment => segment.asString.getOrElse(segment.noSpaces)),
      code
    )
  }
}

/** The server answered, but with errors. Any data it sent alongside them is not read. */
final class GraphQLFailure(val errors: List[GraphQLError])
    extends RuntimeException(errors.map(_.message).mkString("; "))

/** Runs checked operations against a GraphQL endpoint over HTTP.
  *
  * Headers, authentication and tracing belong to the `Client` this is given, as for any other HTTP service.
  */
final class GraphQLClient[F[_]: Concurrent](client: Client[F], endpoint: Uri) {

  /** Runs an operation that takes no variables. */
  def run[D: Decoder](operation: Operation[Unit, D]): F[D] = send(operation, JsonObject.empty)

  def run[V: Encoder.AsObject, D: Decoder](operation: Operation[V, D], variables: V): F[D] =
    // a variable given no value is left out, which is what an `Option` that is `None` means
    send(operation, Encoder.AsObject[V].encodeObject(variables).filter { case (_, value) => !value.isNull })

  private def send[V, D: Decoder](operation: Operation[V, D], variables: JsonObject): F[D] = {
    val body = Json.obj(
      "query"         -> Json.fromString(operation.document),
      "operationName" -> operation.name.fold(Json.Null)(Json.fromString),
      "variables"     -> Json.fromJsonObject(variables)
    )

    client.expect[Json](Request[F](Method.POST, endpoint).withEntity(body)).flatMap { response =>
      response.hcursor.get[Option[List[GraphQLError]]]("errors").map(_.toList.flatten) match {
        case Left(failure)                    => Concurrent[F].raiseError(failure)
        case Right(errors) if errors.nonEmpty => Concurrent[F].raiseError(new GraphQLFailure(errors))
        case Right(_)                         => Concurrent[F].fromEither(response.hcursor.get[D]("data"))
      }
    }
  }
}
