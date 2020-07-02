package lang2

object Parser {

  def makeTree(tokens: List[Token]): Tree[Token] = {
    def loop(
        parenIndex: Int,
        remaining: List[Token],
        trees: Node[Token]
    ): (Int, List[Token], Node[Token]) = {
      (parenIndex, remaining) match {
        case (n, _) if n < 0 => throw new ParseError("Unexpected brackets")
        case (_, Token.LParen :: tail) => {
          val (_, r, tr) = loop(parenIndex + 1, tail, Node.empty[Token])
          (parenIndex, r, tr ::: trees)
        }
        case (_, Token.RParen :: tail) => (parenIndex - 1, tail, trees)
        case (_, token :: tail)        => loop(parenIndex, tail, Leaf(token) :: trees)
        case (i, Nil)                  => (i, Nil, trees)
      }
    }
    loop(0, tokens, Node.empty[Token])._3.compact.reverse
  }

  def parse2(token: List[Token]): Term = {
    val tree: Tree[Token] = makeTree(token)

    ???

  }

  def parse(tokens: List[Token]): Option[Term] = {

    def loop(parenIndex: Int, remaining: List[Token], terms: List[Term]): (Int, List[Token], List[Term]) = {
      (parenIndex, remaining) match {
        case (n, _) if n < 0 => throw new ParseError("Unexpected brackets")
        case (_, Token.LParen :: tail) => {
          val (i, r, args) = loop(parenIndex + 1, tail, Nil)
          (i, r, terms ::: args)
        }
        case (_, Token.RParen :: tail)  => (parenIndex - 1, tail, terms.reverse)
        case (_, Token.Num(v) :: tail)  => loop(parenIndex, tail, Term.Num(v) :: terms)
        case (_, Token.Bool(v) :: tail) => loop(parenIndex, tail, Term.Bool(v) :: terms)
        case (_, Token.Word(v) :: tail) => {
          val (i, r, args) = loop(parenIndex, tail, Nil)
          loop(i, r, Term.Apply(v, args) :: terms)
        }
        // case (_, Let :: tail) => {

        // }
        case (i, Nil) => (i, Nil, terms.reverse)
      }
    }

    loop(0, tokens, Nil)._3.headOption
  }

  class ParseError(message: String) extends Lang2Error("ParseError: " + message)

}
