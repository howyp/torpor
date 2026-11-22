package net.andimiller.torpor.client

import cats.effect.IO
import io.circe.Json
import net.andimiller.torpor.http.TorporHttp
import net.andimiller.torpor.macros.schema
import net.andimiller.torpor.source.*
import org.http4s.{HttpApp, Response, Status}
import org.http4s.circe.*
import org.http4s.client.{Client, UnexpectedStatus}
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

  test("a union read as a sealed trait decodes as the member the server answered with") {
    val home = operation[PetVariables, HomeOfSealed](
      "query Home($id: ID!) { home(pet: $id) { __typename ... on Owner { name } ... on Shelter { name capacity } } }"
    )

    for {
      graphql <- client
      result  <- graphql.run(home, PetVariables(id = "1"))
    } yield assertEquals(result, HomeOfSealed(Some(Homes.Shelter("Paws", 40))))
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

  test("errors in a response that is not a 200 are raised all the same") {
    val onePet  = operation[PetVariables, OnePet]("query P($id: ID!) { pet(id: $id) { name } }")
    val refused = Json.obj("errors" -> Json.arr(Json.obj("message" -> Json.fromString("no such operation"))))
    val graphql = new GraphQLClient[IO](
      Client.fromHttpApp(HttpApp.pure[IO](Response[IO](Status.BadRequest).withEntity(refused))),
      uri"/graphql"
    )

    graphql.run(onePet, PetVariables(id = "1")).attempt.map {
      case Left(raised: GraphQLFailure) => assertEquals(raised.errors.map(_.message), List("no such operation"))
      case other                        => fail(s"expected a GraphQLFailure, got $other")
    }
  }

  test("a failure that is not a GraphQL response is raised with its status") {
    val onePet  = operation[PetVariables, OnePet]("query P($id: ID!) { pet(id: $id) { name } }")
    val graphql = new GraphQLClient[IO](
      Client.fromHttpApp(HttpApp.pure[IO](Response[IO](Status.ServiceUnavailable).withEntity("<html>down</html>"))),
      uri"/graphql"
    )

    graphql.run(onePet, PetVariables(id = "1")).attempt.map {
      case Left(raised: UnexpectedStatus) => assertEquals(raised.status, Status.ServiceUnavailable)
      case other                          => fail(s"expected an UnexpectedStatus, got $other")
    }
  }

  // typed operations: no type is written for the response or the variables

  private val petshop = Api.at("modules/client/src/test/resources/petshop.graphqls")

  test("a typed operation's response is read by field, with the types the schema gives") {
    val pets = petshop.operation(
      """
        query Pets($species: Species, $first: Int) {
          pets(species: $species, first: $first) {
            id
            name
            age
            owner { name }
          }
        }
      """
    )

    for {
      graphql <- client
      data    <- graphql.run(pets(first = Some(2)))
    } yield {
      val names: List[String]          = data.pets.map(_.name)
      val ages: List[Option[Int]]      = data.pets.map(_.age)
      val owners: List[Option[String]] = data.pets.map(_.owner.map(_.name))
      assertEquals(names, List("Rex", "Fido"))
      assertEquals(ages, List(Some(3), None))
      assertEquals(owners, List(Some("Ada"), Some("Ada")))
    }
  }

  test("a typed operation reads an enum as a union of its values") {
    val pets = petshop.operation("{ pets { species } }")

    for {
      graphql <- client
      data    <- graphql.run(pets())
    } yield {
      val species: List["DOG" | "CAT"] = data.pets.map(_.species)
      assertEquals(species, List("DOG", "DOG", "DOG"))
    }
  }

  test("a typed operation with no variables is called with none") {
    val everyPet = petshop.operation("{ pets { name } }")

    for {
      graphql <- client
      data    <- graphql.run(everyPet())
    } yield assertEquals(data.pets.map(_.name), List("Rex", "Fido", "Bo"))
  }

  test("a typed operation reads a union with one member's field as an Option, and a field behind @include too") {
    val home = petshop.operation(
      """
        query Home($id: ID!, $withCapacity: Boolean!) {
          home(pet: $id) {
            __typename
            ... on Owner { name }
            ... on Shelter { name capacity @include(if: $withCapacity) }
          }
        }
      """
    )

    for {
      graphql <- client
      shown   <- graphql.run(home(id = "1", withCapacity = true))
      hidden  <- graphql.run(home(id = "1", withCapacity = false))
    } yield {
      val name: Option[String]  = shown.home.map(_.name)
      val capacity: Option[Int] = shown.home.flatMap(_.capacity)
      assertEquals(shown.home.map(_.__typename), Some("Shelter"))
      assertEquals(name, Some("Paws"))
      assertEquals(capacity, Some(40))
      assertEquals(hidden.home.flatMap(_.capacity), None)
    }
  }

  test("a typed operation reads a leaf as the type the API binds, for a field or for its type") {
    val bound = petshop.read["Pet.id", PetId].read["Int", Long]
    val pets  = bound.operation("{ pets { id age } }")

    for {
      graphql <- client
      data    <- graphql.run(pets())
    } yield {
      val ids: List[PetId]         = data.pets.map(_.id)
      val ages: List[Option[Long]] = data.pets.map(_.age)
      assertEquals(ids, List(PetId("1"), PetId("2"), PetId("3")))
      assertEquals(ages, List(Some(3L), None, Some(7L)))
    }
  }

  test("a variable given as None is left out of the request") {
    val pets = petshop.operation("query Pets($first: Int) { pets(first: $first) { name } }")

    assertEquals(pets(first = None).variables.toMap, Map.empty)
    assertEquals(pets(first = 2).variables.toMap.view.mapValues(_.noSpaces).toMap, Map("first" -> "2"))
  }
}
