package lang2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParserSpec extends AnyFlatSpec with Matchers {

  it should "parser 1" in {

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

  // it should "parser 2" in {

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
  //         Term.Lambda("x", Term.Apply("add", List(Term.Var("n", Term.Var("x"))))),
  //         Term.Apply("f", Term.Num(2))
  //       )
  //     )

  //   term shouldEqual expected

  // }

}
