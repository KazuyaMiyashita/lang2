package lang2

object Parser {

  def parseFromTree(tree: Tree[Token]): Term = {
    tree match {
      case Tree.Leaf(value) =>
        value match {
          case Token.Word(value) => Term.Var(value)
          case Token.Num(value)  => Term.Num(value)
          case Token.Bool(value) => Term.Bool(value)
          case _                 => ???
        }
      case Tree.Node(children) => ???
    }
  }

  def parse(tokens: List[Token]): Term = {
    val tree: Tree[Token] = Tree.makeTree(tokens, Token.LParen, Token.RParen)
    parseFromTree(tree)
  }

  class ParseError(message: String) extends Lang2Error("ParseError: " + message)

}
