package lang2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TokenizerSpec extends AnyFlatSpec with Matchers {

  it should "tokenize 1" in {

    val input = "(add 1 (add 2 3))"
    val token = Tokenizer.tokenize(input)
    val expected = List(
      Token.LParen,
      Token.Word("add"),
      Token.Num(1),
      Token.LParen,
      Token.Word("add"),
      Token.Num(2),
      Token.Num(3),
      Token.RParen,
      Token.RParen
    )

    token shouldEqual expected

  }

  it should "tokenize 2" in {

    val input = "if true 1 0"
    val token = Tokenizer.tokenize(input)
    val expected = List(
      Token.Word("if"),
      Token.Bool(true),
      Token.Num(1),
      Token.Num(0)
    )

    token shouldEqual expected

  }

  it should "tokenize 3" in {

    val input = "(let n 1 (let f (lambda x (add n x)) (f 2)))"
    val token = Tokenizer.tokenize(input)
    val expected = List(
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

    token shouldEqual expected

  }

}
