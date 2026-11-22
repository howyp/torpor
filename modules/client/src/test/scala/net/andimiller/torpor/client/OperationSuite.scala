package net.andimiller.torpor.client

import io.circe.{Decoder, Encoder}

final case class PetsVariables(species: Option[String], first: Option[Int]) derives Encoder.AsObject
final case class Pets(pets: List[Pet]) derives Decoder
final case class Pet(id: String, name: String, age: Option[Int], owner: Option[Owner]) derives Decoder
final case class Owner(name: String) derives Decoder

final case class PetId(value: String) extends AnyVal
object PetId {
  given Decoder[PetId] = Decoder.decodeString.map(PetId(_))
}
final case class PetWithId(id: PetId) derives Decoder

final case class Age(years: Int) extends AnyVal
object Age   {
  given Decoder[Age] = Decoder.decodeInt.map(Age(_))
}
object Names {
  opaque type Name = String
  def apply(name: String): Name = name
  given Decoder[Name]           = Decoder.decodeString
}
final case class PetAgeAndName(name: Names.Name, age: Option[Age]) derives Decoder
type Species = "DOG" | "CAT"
given Decoder[Species] = Literals.oneOf
final case class PetSpecies(species: Species) derives Decoder
final case class PetsSpecies(pets: List[PetSpecies]) derives Decoder
type Bird    = "BIRD" | "CAT"
given Decoder[Bird] = Literals.oneOf
final case class PetBird(species: Bird) derives Decoder
final case class PetsBird(pets: List[PetBird]) derives Decoder
final case class PetsAgeAndName(pets: List[PetAgeAndName]) derives Decoder
final case class OnePetWithId(pet: Option[PetWithId]) derives Decoder

final case class PetVariables(id: String) derives Encoder.AsObject
final case class WrongPetsVariables(species: Option[String], first: Option[String]) derives Encoder.AsObject
final case class HomeX(x: String) derives Decoder
final case class HomeOfX(home: Option[HomeX]) derives Decoder
final case class OptionalPetVariables(id: Option[String]) derives Encoder.AsObject
final case class OnePet(pet: Option[PetName]) derives Decoder
final case class PetName(name: String) derives Decoder
final case class PetAge(age: Int) derives Decoder
final case class OnePetAge(pet: Option[PetAge]) derives Decoder

final case class HomeOf(home: Option[Home]) derives Decoder

object Homes     {
  sealed trait Home
  final case class Owner(name: String)                  extends Home derives Decoder
  final case class Shelter(name: String, capacity: Int) extends Home derives Decoder
  object Home {
    given Decoder[Home] = Decoder.instance { cursor =>
      cursor.get[String]("__typename").flatMap {
        case "Owner"   => cursor.as[Owner]
        case "Shelter" => cursor.as[Shelter]
        case other     => Left(io.circe.DecodingFailure(s"not a Home: $other", cursor.history))
      }
    }
  }
}
final case class HomeOfSealed(home: Option[Homes.Home]) derives Decoder
object OwnerOnly {
  sealed trait Home
  final case class Owner(name: String) extends Home derives Decoder
  object Home {
    given Decoder[Home] = Decoder[Owner].map(identity)
  }
}
final case class HomeOfOwnerOnly(home: Option[OwnerOnly.Home]) derives Decoder
final case class Home(__typename: String, name: String, capacity: Option[Int]) derives Decoder
final case class HomeCapacity(capacity: Int) derives Decoder
final case class HomeOfCapacity(home: Option[HomeCapacity]) derives Decoder

/** What compiles and what does not: the document against the schema, and each type against the document. */
final class OperationSuite extends munit.FunSuite {

  private inline def operation[V, D](inline document: String): Operation[V, D] =
    Operation.checked[V, D]("modules/client/src/test/resources/petshop.graphqls", document)

  test("an operation that fits the schema and its types keeps its document and name") {
    val pets = operation[PetsVariables, Pets](
      "query Pets($species: Species, $first: Int) { pets(species: $species, first: $first) { id name age owner { name } } }"
    )

    assertEquals(pets.name, Some("Pets"))
    assert(pets.document.startsWith("query Pets"))
  }

  test("an operation may be checked against the schema as a constant the build wrote from the file") {
    val pets = Operation.checkedAgainst[Unit, Pets](PetshopSchema.sdl, "{ pets { id name age owner { name } } }")

    assertEquals(pets.name, None)
  }

  test("an operation with no variables takes Unit") {
    val pets = operation[Unit, Pets]("{ pets { id name age owner { name } } }")

    assertEquals(pets.name, None)
  }

  test("a union is read with a field of one member as an Option, and a field of every member as it is") {
    val home = operation[PetVariables, HomeOf](
      "query Home($id: ID!) { home(pet: $id) { __typename ... on Owner { name } ... on Shelter { name capacity } } }"
    )

    assertEquals(home.name, Some("Home"))
  }

  test("a union is read as a sealed trait with a case class per member, each against its own fragment") {
    val home = operation[PetVariables, HomeOfSealed](
      "query Home($id: ID!) { home(pet: $id) { __typename ... on Owner { name } ... on Shelter { name capacity } } }"
    )

    assertEquals(home.name, Some("Home"))
  }

  test("a sealed trait needs __typename selected, a member per type, and each member's fields selected for it") {
    val untagged = compileErrors(
      """Operation.checked[PetVariables, HomeOfSealed]("modules/client/src/test/resources/petshop.graphqls", "query H($id: ID!) { home(pet: $id) { ... on Owner { name } ... on Shelter { name capacity } } }")"""
    )
    val unread   = compileErrors(
      """Operation.checked[PetVariables, HomeOfSealed]("modules/client/src/test/resources/petshop.graphqls", "query H($id: ID!) { home(pet: $id) { __typename ... on Owner { name } ... on Shelter { name } } }")"""
    )
    val missing  = compileErrors(
      """Operation.checked[PetVariables, HomeOfOwnerOnly]("modules/client/src/test/resources/petshop.graphqls", "query H($id: ID!) { home(pet: $id) { __typename ... on Owner { name } } }")"""
    )

    assert(untagged.contains("so the operation must select __typename at query.home"), untagged)
    assert(
      unread.contains("Shelter reads 'capacity', which the operation does not select at query.home as Shelter"),
      unread
    )
    assert(missing.contains("Home has no member for Shelter, which query.home can be"), missing)
  }

  test("a value class is read by its own decoder, like any other scalar") {
    val pet = operation[PetVariables, OnePetWithId]("query P($id: ID!) { pet(id: $id) { id } }")

    assertEquals(pet.name, Some("P"))
  }

  test("a value class is read as the value it wraps, and an opaque type as whatever its codec reads") {
    val pets = operation[Unit, PetsAgeAndName]("{ pets { name age } }")

    assertEquals(pets.name, None)
  }

  test("an enum is read as a union of literals of its values, and not of others") {
    val pets   = operation[Unit, PetsSpecies]("{ pets { species } }")
    val errors = compileErrors(
      """Operation.checked[Unit, PetsBird]("modules/client/src/test/resources/petshop.graphqls", "{ pets { species } }")"""
    )

    assertEquals(pets.name, None)
    assert(errors.contains("but \"BIRD\" is not one of its values: DOG, CAT"), errors)
  }

  test("an operation may interpolate a selection that several operations share") {
    inline val petFields = "id name age owner { name }"

    val interpolated = operation[Unit, Pets](
      s"""
        query All {
          pets { $petFields }
        }
      """
    )
    val joined       = operation[PetVariables, OnePetWithId]("query P($id: ID!) { pet(id: $id) { " + "id" + " } }")
    val trimmed      = operation[Unit, Pets]("""{
                                               |  pets { id name age owner { name } }
                                               |}""".stripMargin)

    assert(interpolated.document.contains("pets { id name age owner { name } }"), interpolated.document)
    assertEquals(joined.name, Some("P"))
    assert(trimmed.document.startsWith("{\n  pets"), trimmed.document)
  }

  test("an operation built from a value the compiler cannot read does not compile") {
    val errors = compileErrors(
      """val fields = List("id").mkString
         Operation.checked[Unit, Pets]("modules/client/src/test/resources/petshop.graphqls", s"{ pets { $fields } }")"""
    )

    assert(errors.contains("the operation must be a constant string"), errors)
  }

  test("a document that is not valid against the schema does not compile") {
    val errors = compileErrors(
      """Operation.checked[Unit, Pets]("modules/client/src/test/resources/petshop.graphqls", "{ pets { id nickname } }")"""
    )

    assert(errors.contains("nickname"), errors)
  }

  test("reading a field the document does not select does not compile") {
    val errors = compileErrors(
      """Operation.checked[Unit, Pets]("modules/client/src/test/resources/petshop.graphqls", "{ pets { id name age } }")"""
    )

    assert(errors.contains("Pet reads 'owner', which the operation does not select at query.pets[]"), errors)
  }

  test("selecting a field that is never read does not compile") {
    val errors = compileErrors(
      """Operation.checked[PetVariables, OnePet]("modules/client/src/test/resources/petshop.graphqls", "query P($id: ID!) { pet(id: $id) { name age } }")"""
    )

    assert(errors.contains("query.pet.age is selected, but PetName never reads it"), errors)
  }

  test("reading a nullable field as a plain value does not compile") {
    val errors = compileErrors(
      """Operation.checked[PetVariables, OnePetAge]("modules/client/src/test/resources/petshop.graphqls", "query P($id: ID!) { pet(id: $id) { age } }")"""
    )

    assert(errors.contains("query.pet.age is nullable in the schema, so it is read as an Option, not Int"), errors)
  }

  test("reading a field of one union member as a plain value does not compile") {
    val errors = compileErrors(
      """Operation.checked[PetVariables, HomeOfCapacity]("modules/client/src/test/resources/petshop.graphqls", "query H($id: ID!) { home(pet: $id) { ... on Shelter { capacity } } }")"""
    )

    assert(errors.contains("query.home.capacity is only in the response for some types or conditions"), errors)
  }

  test("an Option for a variable that must be given a value does not compile") {
    val errors = compileErrors(
      """Operation.checked[OptionalPetVariables, OnePet]("modules/client/src/test/resources/petshop.graphqls", "query P($id: ID!) { pet(id: $id) { name } }")"""
    )

    assert(errors.contains("'$id' must be given a value, so OptionalPetVariables.id cannot be an Option"), errors)
  }

  test("variables that do not match the ones the operation declares do not compile") {
    val errors = compileErrors(
      """Operation.checked[PetsVariables, OnePet]("modules/client/src/test/resources/petshop.graphqls", "query P($id: ID!) { pet(id: $id) { name } }")"""
    )

    assert(errors.contains("PetsVariables has no field for the variable '$id'"), errors)
    assert(
      errors.contains("PetsVariables has a field 'species', but the operation declares no variable '$species'"),
      errors
    )
  }

  test("a variable field of the wrong type does not compile") {
    val errors = compileErrors(
      """Operation.checked[WrongPetsVariables, Pets]("modules/client/src/test/resources/petshop.graphqls", "query Pets($species: Species, $first: Int) { pets(species: $species, first: $first) { id name age owner { name } } }")"""
    )

    assert(
      errors.contains(
        "WrongPetsVariables.first is a Int in the schema, so it is written as BigInt or Int or Long, not String"
      ),
      errors
    )
  }

  test("one response key with two types across fragments does not compile") {
    val errors = compileErrors(
      """Operation.checked[PetVariables, HomeOfX]("modules/client/src/test/resources/petshop.graphqls", "query H($id: ID!) { home(pet: $id) { ... on Owner { x: name } ... on Shelter { x: capacity } } }")"""
    )

    assert(
      errors.contains("query.home.x is String! on Owner and Int! on Shelter, so it cannot be read as one field"),
      errors
    )
  }

  test("a schema file that cannot be found does not compile") {
    val errors = compileErrors("""Operation.checked[Unit, Pets]("nowhere.graphqls", "{ pets { id } }")""")

    assert(errors.contains("cannot find the schema file 'nowhere.graphqls'"), errors)
  }
}
