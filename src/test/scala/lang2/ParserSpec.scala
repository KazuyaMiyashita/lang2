package lang2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParserSpec extends AnyFlatSpec with Matchers {

  it should "makeTree 1" in {
    val tokens: List[Token] = List(
      Token.LParen,
      Token.Word("a"),
      Token.LParen,
      Token.Word("b"),
      Token.Word("c"),
      Token.RParen,
      Token.RParen
    )

    val tree = Parser.makeTree(tokens)
    val expected = Node(
      Leaf(Token.Word("a")),
      Node(
        Leaf(Token.Word("b")),
        Leaf(Token.Word("c"))
      )
    )

    tree shouldEqual expected

  }

  it should "makeTree 2" in {

    // "(let n 1 (let f (lambda x (add n x)) (f 2)))"
    val tokens = List(
      Token.LParen,
      Token.Let,
      Token.Word("n"),
      Token.Num(1),
      Token.LParen,
      Token.Let,
      Token.Word("f"),
      Token.LParen,
      Token.Lambda,
      Token.Word("x"),
      Token.LParen,
      Token.Word("add"),
      Token.Word("n"),
      Token.Word("x"),
      Token.RParen,
      Token.RParen,
      Token.LParen,
      Token.Word("f"),
      Token.Num(2),
      Token.RParen,
      Token.RParen,
      Token.RParen
    )

    val tree = Parser.makeTree(tokens)

    val expected: Tree[Token] =
      Node(
        Leaf(Token.Let),
        Leaf(Token.Word("n")),
        Leaf(Token.Num(1)),
        Node(
          Leaf(Token.Let),
          Leaf(Token.Word("f")),
          Node(
            Leaf(Token.Lambda),
            Leaf(Token.Word("x")),
            Node(
              Leaf(Token.Word("add")),
              Leaf(Token.Word("n")),
              Leaf(Token.Word("x"))
            )
          ),
          Node(
            Leaf(Token.Word("f")),
            Leaf(Token.Num(2))
          )
        )
      )

    tree shouldEqual expected

  }

  it should "parse 1" in {

    val tokens = List(
      Token.LParen,
      Token.Word("add"),
      Token.Num(1),
      Token.LParen,
      Token.Word("mul"),
      Token.Num(2),
      Token.Num(3),
      Token.RParen,
      Token.RParen
    )
    val term = Parser.parse(tokens).get
    val expected = Term.Apply(
      "add",
      List(
        Term.Num(1),
        Term.Apply(
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

  it should "parse 2" in {
    // "let n 1 n"
    val tokens = List(
      Token.Let,
      Token.Num(1),
      Token.Word("n")
    )
    val term     = Parser.parse(tokens).get
    val expected = Term.Let("n", Term.Num(1), Term.Var("n"))
    term shouldEqual expected
  }

  it should "parse 3" in {

    // "(let n 1 (let f (lambda x (add n x)) (f 2)))"
    val tokens = List(
      Token.LParen,
      Token.Let,
      Token.Word("n"),
      Token.Num(1),
      Token.LParen,
      Token.Let,
      Token.Word("f"),
      Token.LParen,
      Token.Lambda,
      Token.Word("x"),
      Token.LParen,
      Token.Word("add"),
      Token.Word("n"),
      Token.Word("x"),
      Token.RParen,
      Token.RParen,
      Token.LParen,
      Token.Word("f"),
      Token.Num(2),
      Token.RParen,
      Token.RParen,
      Token.RParen
    )
    val term = Parser.parse(tokens).get
    val expected =
      Term.Let(
        "n",
        Term.Num(1),
        Term.Let(
          "f",
          Term.Lambda("x", Term.Apply("add", List(Term.Var("n"), Term.Var("x")))),
          Term.Apply("f", List(Term.Num(2)))
        )
      )

    term shouldEqual expected

  }

}
