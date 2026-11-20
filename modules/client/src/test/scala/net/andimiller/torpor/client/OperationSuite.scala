package net.andimiller.torpor.client

import io.circe.{Decoder, Encoder}

final case class PetsVariables(species: Option[String], first: Option[Int]) derives Encoder.AsObject
final case class Pets(pets: List[Pet]) derives Decoder
final case class Pet(id: String, name: String, age: Option[Int], owner: Option[Owner]) derives Decoder
final case class Owner(name: String) derives Decoder

final case class PetVariables(id: String) derives Encoder.AsObject
final case class OptionalPetVariables(id: Option[String]) derives Encoder.AsObject
final case class OnePet(pet: Option[PetName]) derives Decoder
final case class PetName(name: String) derives Decoder
final case class PetAge(age: Int) derives Decoder
final case class OnePetAge(pet: Option[PetAge]) derives Decoder

final case class HomeOf(home: Option[Home]) derives Decoder
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

  test("a schema file that cannot be found does not compile") {
    val errors = compileErrors("""Operation.checked[Unit, Pets]("nowhere.graphqls", "{ pets { id } }")""")

    assert(errors.contains("cannot find the schema file 'nowhere.graphqls'"), errors)
  }
}
