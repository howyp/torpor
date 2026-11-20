package net.andimiller.torpor.client

import cats.effect.IO
import io.circe.Json
import net.andimiller.torpor.http.TorporHttp
import net.andimiller.torpor.macros.schema
import net.andimiller.torpor.source.*
import org.http4s.client.Client
import org.http4s.implicits.*

/** Checked operations run against a real torpor server, in process. */
final class GraphQLClientSuite extends munit.CatsEffectSuite {

  private val sdl = schema"""enum Species { DOG CAT }
    |type Pet { id: ID! name: String! species: Species! age: Int owner: Owner }
    |type Owner { name: String! pets: [Pet!]! }
    |type Shelter { name: String! capacity: Int! }
    |union Home = Owner | Shelter
    |extend type Query {
    |  pets(species: Species, first: Int): [Pet!]!
    |  pet(id: ID!): Pet
    |  home(pet: ID!): Home
    |}"""

  private def pet(id: String, name: String, age: Option[Int]): Json =
    Json.obj(
      "id"      -> Json.fromString(id),
      "name"    -> Json.fromString(name),
      "species" -> Json.fromString("DOG"),
      "age"     -> age.fold(Json.Null)(Json.fromInt),
      "owner"   -> Json.obj("name" -> Json.fromString("Ada"), "pets" -> Json.arr())
    )

  private val pets = List(pet("1", "Rex", Some(3)), pet("2", "Fido", None), pet("3", "Bo", Some(7)))

  private val source = Source[IO]("pets", sdl)(
    resolver.root[IO](sdl.Query.pets)(args => IO.pure(pets.take(args.first.getOrElse(pets.size)))),
    resolver.root[IO](sdl.Query.pet) { args =>
      if (args.id == "broken") IO.raiseError(new RuntimeException("the kennel is closed"))
      else IO.pure(pets.find(_.hcursor.get[String]("id").contains(args.id)))
    },
    resolver.root[IO](sdl.Query.home) { _ =>
      IO.pure(
        Option(
          Json.obj(
            "__typename" -> Json.fromString("Shelter"),
            "name"       -> Json.fromString("Paws"),
            "capacity"   -> Json.fromInt(40)
          )
        )
      )
    }
  )

  private val client: IO[GraphQLClient[IO]] =
    Torpor[IO](source).startup.map { (torpor, schema) =>
      new GraphQLClient[IO](Client.fromHttpApp(TorporHttp.routes(torpor, schema).orNotFound), uri"/graphql")
    }

  private inline def operation[V, D](inline document: String): Operation[V, D] =
    Operation.checked[V, D]("modules/client/src/test/resources/petshop.graphqls", document)

  test("an operation's response is read as its type, with a variable that is None left out") {
    val operations = operation[PetsVariables, Pets](
      "query Pets($species: Species, $first: Int) { pets(species: $species, first: $first) { id name age owner { name } } }"
    )

    for {
      graphql <- client
      result  <- graphql.run(operations, PetsVariables(species = None, first = Some(2)))
    } yield assertEquals(
      result,
      Pets(
        List(
          Pet(id = "1", name = "Rex", age = Some(3), owner = Some(Owner("Ada"))),
          Pet(id = "2", name = "Fido", age = None, owner = Some(Owner("Ada")))
        )
      )
    )
  }

  test("an operation with no variables runs without any") {
    val everyPet = operation[Unit, Pets]("{ pets { id name age owner { name } } }")

    for {
      graphql <- client
      result  <- graphql.run(everyPet)
    } yield assertEquals(result.pets.map(_.name), List("Rex", "Fido", "Bo"))
  }

  test("a union member is read with the fields its fragments select") {
    val home = operation[PetVariables, HomeOf](
      "query Home($id: ID!) { home(pet: $id) { __typename ... on Owner { name } ... on Shelter { name capacity } } }"
    )

    for {
      graphql <- client
      result  <- graphql.run(home, PetVariables(id = "1"))
    } yield assertEquals(result, HomeOf(Some(Home(__typename = "Shelter", name = "Paws", capacity = Some(40)))))
  }

  test("errors in the response are raised with their messages and paths") {
    val onePet = operation[PetVariables, OnePet]("query P($id: ID!) { pet(id: $id) { name } }")

    for {
      graphql <- client
      failure <- graphql.run(onePet, PetVariables(id = "broken")).attempt
    } yield failure match {
      case Left(raised: GraphQLFailure) => assertEquals(raised.errors.map(_.path), List(List("pet")))
      case other                        => fail(s"expected a GraphQLFailure, got $other")
    }
  }
}
