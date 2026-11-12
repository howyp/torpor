package net.andimiller.torpor.source

import cats.effect.IO
import net.andimiller.torpor.macros.schema

final class SourceSuite extends munit.FunSuite {

  private val pets = schema"""type Pet { id: ID! name: String! }
                             |extend type Query { petName(id: ID!): String }
                             |"""

  private val petsSource = Source[IO]("pets", pets.text)(
    resolver.root[IO](pets.Query.petName) { _ => IO.pure(Option("Rex")) }
  )

  test("something that is not a resolver does not compile") {
    val errors = compileErrors("""Source[IO]("pets", pets.text)("not a resolver", 1)""")
    assert(errors.contains("Match type reduction failed"), errors)
  }

  test("a source takes its resolvers once") {
    val errors = compileErrors("""
      petsSource(resolver.root[IO](pets.Query.petName) { _ => IO.pure(Option.empty[String]) })
    """)
    assert(errors.contains("EmptyTuple"), errors)
  }
}
