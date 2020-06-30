package lang2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import Term._
import Type._

class TypeCheckerSpec extends AnyFlatSpec with Matchers {

  it should "typeChecker 1" in {

    val env = Environment(
      Map(
        "add" -> Func.add,
        "mul" -> Func.mul
      )
    )

    val term = Apply(
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
    val reuslt   = TypeChecker.check(term, env)
    val expected = NumType

    reuslt shouldEqual expected

  }

  it should "typeChecker 2" in {

    val env = Environment(
      Map(
        "ifnum" -> Func.ifnum
      )
    )

    val term = Apply(
      "ifnum",
      List(
        Bool(true),
        Num(1),
        Num(0)
      )
    )

    val reuslt   = TypeChecker.check(term, env)
    val expected = NumType

    reuslt shouldEqual expected

  }

  it should "typeChecker 3" in {

    val env = Environment(
      Map(
        "ifnum" -> Func.ifnum
      )
    )

    val term = Apply(
      "ifnum",
      List(
        Bool(true),
        Num(1),
        Bool(false)
      )
    )

    assertThrows[TypeChecker.IllegalArgumentTypeError] {
      TypeChecker.check(term, env)
    }

  }

  it should "typeChecker 4" in {

    val env = Environment(
      Map(
        "echonum" -> Func.echonum,
        "mul"     -> Func.mul
      )
    )

    val term = Block(
      List(
        Apply("echonum", List(Num(1))),
        Bool(true),
        Apply("mul", List(Num(2), Num(3)))
      )
    )

    val reuslt   = TypeChecker.check(term, env)
    val expected = NumType

    reuslt shouldEqual expected

  }

}
