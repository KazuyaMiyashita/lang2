package lang2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import Term._
import Token._

class ParserSpec extends AnyFlatSpec with Matchers {

  it should "parser 1" in {

    val tokens = List(
      LParen,
      Command("add"),
      Numeric(1),
      LParen,
      Command("mul"),
      Numeric(2),
      Numeric(3),
      RParen,
      RParen
    )
    val expr = Parser.parse(tokens).get
    val expected = Apply(
      "add",
      List(
        Num(1),
        Apply(
          "mul",
          List(
            Num(2),
            Num(3)
          )
        )
      )
    )

    expr shouldEqual expected

  }

}
