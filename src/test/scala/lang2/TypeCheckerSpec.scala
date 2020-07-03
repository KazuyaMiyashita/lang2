package lang2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypeCheckerSpec extends AnyFlatSpec with Matchers {

  it should "typeChecker 1" in {

    val env = Environment(
      Map(
        "add" -> Func.add,
        "mul" -> Func.mul
      )
    )

    val term = Term.Function(
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
    val reuslt   = TypeChecker.check(term, env)
    val expected = Type.Num

    reuslt shouldEqual expected

  }

  it should "typeChecker 2" in {

    val env = Environment(
      Map(
        "ifnum" -> Func.ifnum
      )
    )

    val term = Term.Function(
      "ifnum",
      List(
        Term.Bool(true),
        Term.Num(1),
        Term.Num(0)
      )
    )

    val reuslt   = TypeChecker.check(term, env)
    val expected = Type.Num

    reuslt shouldEqual expected

  }

  it should "typeChecker 3" in {

    val env = Environment(
      Map(
        "ifnum" -> Func.ifnum
      )
    )

    val term = Term.Function(
      "ifnum",
      List(
        Term.Bool(true),
        Term.Num(1),
        Term.Bool(false)
      )
    )

    assertThrows[TypeChecker.IllegalArgumentTypeError] {
      TypeChecker.check(term, env)
    }

  }

}
