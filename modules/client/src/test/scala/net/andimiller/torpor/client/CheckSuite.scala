package net.andimiller.torpor.client

import net.andimiller.torpor.client.Shape.*
import net.andimiller.torpor.schema.{Schema, SdlFragment}

/** The rules that decide whether an operation fits its types, without the compiler. */
final class CheckSuite extends munit.FunSuite {

  private val schema =
    Schema.merge(
      List(
        SdlFragment(
          "library",
          """enum Genre { FICTION SCIENCE }
            |scalar Date
            |interface Item { id: ID! title: String! }
            |type Book implements Item { id: ID! title: String! pages: Int! genre: Genre! published: Date rating: Float }
            |type Film implements Item { id: ID! title: String! minutes: Int! }
            |type Shelf { name: String! items: [Item!]! featured: Item tags: [String]! }
            |type Query { shelf(name: String!): Shelf shelves(first: Int = 10): [Shelf!]! }
            |type Mutation { rename(name: String!, to: String!): Shelf }
            |""".stripMargin
        )
      )
    ) match {
      case Right(merged) => merged
      case Left(errors)  => fail(errors.toChain.toList.map(_.render).mkString("; "))
    }

  private val noVariables = Leaf("Unit")

  private def problems(document: String, variables: Shape, data: Shape): List[String] =
    Check.operation(schema, document, variables, data).left.getOrElse(Nil)

  private def shelves(shelf: Shape): Shape = Record("Shelves", List("shelves" -> Many(shelf)))

  test("an operation that fits has no problems and reports its name") {
    val data = shelves(Record("Shelf", List("name" -> Leaf("String"))))

    assertEquals(Check.operation(schema, "query All { shelves { name } }", noVariables, data), Right(Some("All")))
  }

  test("a non-null field may be read as an Option") {
    val data = shelves(Record("Shelf", List("name" -> Optional(Leaf("String")))))

    assertEquals(problems("{ shelves { name } }", noVariables, data), Nil)
  }

  test("a list of nullable elements is a collection of Options") {
    val plain    = shelves(Record("Shelf", List("tags" -> Many(Leaf("String")))))
    val optional = shelves(Record("Shelf", List("tags" -> Many(Optional(Leaf("String"))))))

    assertEquals(
      problems("{ shelves { tags } }", noVariables, plain),
      List("query.shelves[].tags[] is nullable in the schema, so it is read as an Option, not String")
    )
    assertEquals(problems("{ shelves { tags } }", noVariables, optional), Nil)
  }

  test("a list read as a single value is a problem") {
    val data = Record("Shelves", List("shelves" -> Record("Shelf", List("name" -> Leaf("String")))))

    assertEquals(
      problems("{ shelves { name } }", noVariables, data),
      List("query.shelves is a list in the schema, so it is read as a collection, not Shelf")
    )
  }

  test("built-in scalars are read as the Scala types that hold them") {
    def item(pages: Shape, rating: Shape): Shape =
      shelves(
        Record(
          "Shelf",
          List("items" -> Many(Record("Item", List("pages" -> Optional(pages), "rating" -> Optional(rating)))))
        )
      )
    val document                                 = "{ shelves { items { ... on Book { pages rating } } } }"

    assertEquals(problems(document, noVariables, item(pages = Leaf("Long"), rating = Leaf("BigDecimal"))), Nil)
    assertEquals(
      problems(document, noVariables, item(pages = Leaf("String"), rating = Leaf("Int"))),
      List(
        "query.shelves[].items[].pages is a Int in the schema, so it is read as BigInt or Int or Long, not String",
        "query.shelves[].items[].rating is a Float in the schema, so it is read as BigDecimal or Double or Float, not Int"
      )
    )
  }

  test("an enum, an id and a custom scalar are read as any type that is not a case class or a collection") {
    def book(genre: Shape): Shape =
      shelves(
        Record(
          "Shelf",
          List(
            "items" -> Many(
              Record(
                "Book",
                List("id" -> Leaf("BookId"), "genre" -> Optional(genre), "published" -> Optional(Leaf("LocalDate")))
              )
            )
          )
        )
      )
    val document                  = "{ shelves { items { id ... on Book { genre published } } } }"

    assertEquals(problems(document, noVariables, book(genre = Leaf("Genre"))), Nil)
    assertEquals(
      problems(document, noVariables, book(genre = Record("Genre", Nil))),
      List("query.shelves[].items[].genre is the scalar or enum Genre in the schema, so it cannot be read as Genre")
    )
  }

  test("an interface's own field is always there, and one implementer's field is not") {
    def item(pages: Shape): Shape =
      shelves(Record("Shelf", List("items" -> Many(Record("Item", List("title" -> Leaf("String"), "pages" -> pages))))))
    val document                  = "{ shelves { items { title ... on Book { pages } } } }"

    assertEquals(problems(document, noVariables, item(pages = Optional(Leaf("Int")))), Nil)
    assertEquals(
      problems(document, noVariables, item(pages = Leaf("Int"))),
      List(
        "query.shelves[].items[].pages is only in the response for some types or conditions, so it is read as an Option, not Int"
      )
    )
  }

  test("a field selected for every implementer is always there") {
    val data     = shelves(Record("Shelf", List("items" -> Many(Record("Item", List("id" -> Leaf("String")))))))
    val document = "{ shelves { items { ... on Book { id } ... on Film { id } } } }"

    assertEquals(problems(document, noVariables, data), Nil)
  }

  test("a named fragment is checked like an inline one") {
    val data     = shelves(Record("Shelf", List("items" -> Many(Record("Item", List("minutes" -> Leaf("Int")))))))
    val document = "query Q { shelves { items { ...Length } } } fragment Length on Film { minutes }"

    assertEquals(
      problems(document, noVariables, data),
      List(
        "query.shelves[].items[].minutes is only in the response for some types or conditions, so it is read as an Option, not Int"
      )
    )
  }

  test("a field behind @include may be missing") {
    val data      = shelves(Record("Shelf", List("name" -> Leaf("String"))))
    val variables = Record("Variables", List("named" -> Leaf("Boolean")))

    assertEquals(
      problems("query Q($named: Boolean!) { shelves { name @include(if: $named) } }", variables, data),
      List(
        "query.shelves[].name is only in the response for some types or conditions, so it is read as an Option, not String"
      )
    )
  }

  test("an alias is the name the field is read by") {
    val data = Record("Two", List("fiction" -> Optional(Record("Shelf", List("name" -> Leaf("String"))))))

    assertEquals(problems("""{ fiction: shelf(name: "fiction") { name } }""", noVariables, data), Nil)
  }

  test("every read field is selected, and every selected field is read") {
    val data = shelves(Record("Shelf", List("name" -> Leaf("String"), "featured" -> Optional(Leaf("String")))))

    assertEquals(
      problems("{ shelves { name tags } }", noVariables, data),
      List(
        "query.shelves[].tags is selected, but Shelf never reads it",
        "Shelf reads 'featured', which the operation does not select at query.shelves[]"
      )
    )
  }

  test("__typename may be selected without being read") {
    val data = shelves(Record("Shelf", List("name" -> Leaf("String"))))

    assertEquals(problems("{ shelves { __typename name } }", noVariables, data), Nil)
  }

  test("variables are a case class with one field per declared variable") {
    val data      = Record("OneShelf", List("shelf" -> Optional(Record("Shelf", List("name" -> Leaf("String"))))))
    val document  = "query Q($name: String!) { shelf(name: $name) { name } }"
    val variables = Record("Variables", List("name" -> Leaf("String")))

    assertEquals(problems(document, variables, data), Nil)
    assertEquals(
      problems(document, noVariables, data),
      List("the operation declares variables, so they are written as a case class, not Unit")
    )
    assertEquals(
      problems(document, Record("Variables", List("title" -> Leaf("String"))), data),
      List(
        "Variables has no field for the variable '$name'",
        "Variables has a field 'title', but the operation declares no variable '$title'"
      )
    )
  }

  test("a variable that must be given a value is not an Option, unless it has a default") {
    val data = shelves(Record("Shelf", List("name" -> Leaf("String"))))

    assertEquals(
      problems(
        "query Q($first: Int!) { shelves(first: $first) { name } }",
        Record("Variables", List("first" -> Optional(Leaf("Int")))),
        data
      ),
      List("'$first' must be given a value, so Variables.first cannot be an Option")
    )
    assertEquals(
      problems(
        "query Q($first: Int! = 5) { shelves(first: $first) { name } }",
        Record("Variables", List("first" -> Optional(Leaf("Int")))),
        data
      ),
      Nil
    )
  }

  test("a mutation is read from the mutation type") {
    val data      = Record("Renamed", List("rename" -> Optional(Record("Shelf", List("name" -> Leaf("String"))))))
    val variables = Record("Variables", List("name" -> Leaf("String"), "to" -> Leaf("String")))

    assertEquals(
      problems("mutation M($name: String!, $to: String!) { rename(name: $name, to: $to) { name } }", variables, data),
      Nil
    )
  }

  test("a document that is not valid against the schema reports the validator's errors") {
    val data = shelves(Record("Shelf", List("name" -> Leaf("String"))))

    assert(problems("{ shelves { nickname } }", noVariables, data).exists(_.contains("nickname")))
  }
}
