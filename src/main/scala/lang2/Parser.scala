package lang2

object Parser {

  def parseTree(tree: Tree[Token]): Term = {
    tree match {
      case Tree.Leaf(value) =>
        value match {
          case Token.Word(value) => Term.Var(value)
          case Token.Num(value)  => Term.Num(value)
          case Token.Bool(value) => Term.Bool(value)
          case _                 => ???
        }
      case Tree.Node(Tree.Leaf(Token.Let) :: Tree.Leaf(Token.Word(variableName)) :: init :: block :: Nil) =>
        Term.Let(variableName, parseTree(init), parseTree(block))
      case Tree.Node(Tree.Leaf(Token.Let) :: _)                   => throw new ParseError("Unexpected let")
      case Tree.Node(Tree.Leaf(Token.Word(functionName)) :: args) => Term.Function(functionName, args.map(parseTree))
      case _                                                      => ???
    }
  }

  def parse(tokens: List[Token]): Term = {
    val tree: Tree[Token] = Tree.makeTree(tokens, Token.LParen, Token.RParen)
    parseTree(tree)
  }

  class ParseError(message: String) extends Lang2Error("ParseError: " + message)

}
