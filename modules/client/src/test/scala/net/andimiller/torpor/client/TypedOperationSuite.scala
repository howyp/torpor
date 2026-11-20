package net.andimiller.torpor.client

import io.circe.Json

/** What compiles and what does not when an operation's types are written by the macro. */
final class TypedOperationSuite extends munit.FunSuite {

  private inline val library = "modules/client/src/test/resources/library.graphqls"

  private val shelves = Operation.typed(
    library,
    """
      query Shelves($filter: ShelfFilter, $first: Int = 10, $names: [String!]!) {
        shelves(filter: $filter, first: $first, names: $names) {
          name
          published
        }
      }
    """
  )

  test("an operation keeps its document and name") {
    assertEquals(shelves.name, Some("Shelves"))
    assert(shelves.document.contains("query Shelves"))
  }

  test("variables are given by name, and one with a default or a nullable type may be left out") {
    val request = shelves(names = List("fiction"))

    assertEquals(request.variables.toMap, Map("names" -> Json.arr(Json.fromString("fiction"))))
  }

  test("an input object is written with input(…), with fields it does not need left out") {
    val request = shelves(names = List("fiction"), filter = input(genre = "FICTION", minItems = Option.empty[Int]))

    assertEquals(
      request.variables.toMap.get("filter"),
      Some(Json.obj("genre" -> Json.fromString("FICTION")))
    )
  }

  test("an input object may hold another, and a list of them") {
    val request = shelves(
      names = List("fiction"),
      filter = input(genre = "FICTION", or = List(input(genre = "SCIENCE"), input(genre = "FICTION", minItems = 3)))
    )

    assertEquals(
      request.variables.toMap.get("filter").map(_.noSpaces),
      Some("""{"genre":"FICTION","or":[{"genre":"SCIENCE"},{"genre":"FICTION","minItems":3}]}""")
    )
  }

  test("a custom scalar is read as Json unless a binding names its type") {
    val published: Option[Json] = Option.empty[shelves.Data].flatMap(_.shelves.headOption).flatMap(_.published)

    assertEquals(published, None)
  }

  test("reading a field the operation does not select does not compile") {
    val errors = compileErrors("""(data: shelves.Data) => data.shelves.map(_.tags)""")

    assert(errors.contains("tags"), errors)
  }

  test("leaving out a variable that must be given a value does not compile") {
    val errors = compileErrors("""shelves(first = 2)""")

    assert(errors.contains("$names must be given a value"), errors)
  }

  test("a variable the operation does not declare does not compile") {
    val errors = compileErrors("""shelves(names = List("a"), limit = 2)""")

    assert(errors.contains("the operation declares no variable '$limit'"), errors)
  }

  test("a value of the wrong kind for a variable does not compile") {
    val errors = compileErrors("""shelves(names = "fiction", first = "two")""")

    assert(errors.contains("$names is a list in the schema, so it is written as a collection, not String"), errors)
    assert(
      errors.contains("$first is a Int in the schema, so it is written as BigInt or Int or Long, not String"),
      errors
    )
  }

  test("an input object with a field its type does not have does not compile") {
    val errors = compileErrors("""shelves(names = List("a"), filter = input(genre = "FICTION", colour = "red"))""")

    assert(errors.contains("$filter has a field 'colour', which the input type ShelfFilter does not have"), errors)
  }

  test("an input object without a field its type requires does not compile") {
    val errors = compileErrors("""shelves(names = List("a"), filter = input(minItems = 3))""")

    assert(errors.contains("$filter.genre must be given a value"), errors)
  }

  test("a plain value where an input object is wanted does not compile") {
    val errors = compileErrors("""shelves(names = List("a"), filter = "FICTION")""")

    assert(
      errors.contains("$filter is the input type ShelfFilter in the schema, so it is written with input(…)"),
      errors
    )
  }

  test("a document that is not valid against the schema does not compile") {
    val errors = compileErrors(
      """Operation.typed("modules/client/src/test/resources/library.graphqls", "{ shelves(names: []) { nickname } }")"""
    )

    assert(errors.contains("nickname"), errors)
  }

  test("a leaf bound to a type with no Decoder does not compile") {
    val errors = compileErrors(
      """final class Published
         given net.andimiller.torpor.core.Scalar["Date", Published]()
         Operation.typed("modules/client/src/test/resources/library.graphqls", "{ shelves(names: []) { published } }")"""
    )

    assert(errors.contains("which has no Decoder in scope"), errors)
  }
}
