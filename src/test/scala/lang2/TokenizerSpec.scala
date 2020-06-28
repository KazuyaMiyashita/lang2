package lang2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import Token._

class TokenizerSpec extends AnyFlatSpec with Matchers {

  it should "tokenize 1" in {

    val input = "(add 1 (add 2 3))"
    val token = Tokenizer.tokenize(input)
    val expected = List(
      LParen,
      Command("add"),
      Numeric(1),
      LParen,
      Command("add"),
      Numeric(2),
      Numeric(3),
      RParen,
      RParen
    )

    token shouldEqual expected

  }

}