package lang2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParserSpec extends AnyFlatSpec with Matchers {

  it should "parse 1" in {

    val tree: Tree[Token] = Tree.Leaf(Token.Num(1))
    val term              = Parser.parseTree(tree)
    val expected          = Term.Num(1)

    term shouldEqual expected

  }

  it should "parse 2" in {

    val tree: Tree[Token] = Tree.Node(
      Tree.Leaf(Token.Word("add")),
      Tree.Leaf(Token.Num(1)),
      Tree.Node(
        Tree.Leaf(Token.Word("mul")),
        Tree.Leaf(Token.Num(2)),
        Tree.Leaf(Token.Num(3))
      )
    )
    val term = Parser.parseTree(tree)
    val expected = Term.Function(
      "add",
      List(
        Term.Num(1),
        Term.Function(
          "mul",
          List(
            Term.Num(2),
            Term.Num(3)
          )
        )
      )
    )

    term shouldEqual expected

  }

  it should "parse 3" in {
    // "let n 1 n"
    val tree: Tree[Token] = Tree.Node(
      Tree.Leaf(Token.Let),
      Tree.Leaf(Token.Word("n")),
      Tree.Leaf(Token.Num(1)),
      Tree.Leaf(Token.Word("n"))
    )
    val term     = Parser.parseTree(tree)
    val expected = Term.Let("n", Term.Num(1), Term.Var("n"))
    term shouldEqual expected
  }

  // it should "parse 3" in {

  //   // "(let n 1 (let f (lambda x (add n x)) (f 2)))"
  //   val tokens = List(
  //     Token.LParen,
  //     Token.Let,
  //     Token.Word("n"),
  //     Token.Num(1),
  //     Token.LParen,
  //     Token.Let,
  //     Token.Word("f"),
  //     Token.LParen,
  //     Token.Lambda,
  //     Token.Word("x"),
  //     Token.LParen,
  //     Token.Word("add"),
  //     Token.Word("n"),
  //     Token.Word("x"),
  //     Token.RParen,
  //     Token.RParen,
  //     Token.LParen,
  //     Token.Word("f"),
  //     Token.Num(2),
  //     Token.RParen,
  //     Token.RParen,
  //     Token.RParen
  //   )
  //   val term = Parser.parse(tokens).get
  //   val expected =
  //     Term.Let(
  //       "n",
  //       Term.Num(1),
  //       Term.Let(
  //         "f",
  //         Term.Lambda("x", Term.Function("add", List(Term.Var("n"), Term.Var("x")))),
  //         Term.Function("f", List(Term.Num(2)))
  //       )
  //     )

  //   term shouldEqual expected

  // }

}
