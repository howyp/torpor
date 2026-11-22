package net.andimiller.torpor.client

/** The shape of a Scala type, as far as a GraphQL document can say anything about it.
  *
  * The macro reads a shape off each type an operation names, and [[Check]] compares it with the document. Keeping the
  * comparison free of compiler types is what lets it be tested as ordinary code.
  */
enum Shape {

  /** `Option[A]`. */
  case Optional(of: Shape)

  /** A collection of `A`. */
  case Many(of: Shape)

  /** A case class, by its fields in order. */
  case Record(name: String, fields: List[(String, Shape)])

  /** Anything else: a primitive, or a type whose own codec decides what it reads, such as an id or an enum. */
  case Leaf(name: String)

  /** A type the document can say nothing about, so it fits any leaf: an opaque type, whose own codec decides, or
    * `Nothing`, the type of what `None` holds and `Nil` contains.
    */
  case Trusted(name: String)

  def render: String = this match {
    case Optional(of)    => s"Option[${of.render}]"
    case Many(of)        => s"List[${of.render}]"
    case Record(name, _) => name
    case Leaf(name)      => name
    case Trusted(name)   => name
  }
}
