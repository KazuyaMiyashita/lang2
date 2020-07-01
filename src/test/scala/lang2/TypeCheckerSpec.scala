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

    val term = Term.Apply(
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

    val term = Term.Apply(
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

    val term = Term.Apply(
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

  it should "typeChecker 4" in {

    val env = Environment(
      Map(
        "echonum" -> Func.echonum,
        "mul"     -> Func.mul
      )
    )

    val term = Term.Block(
      List(
        Term.Apply("echonum", List(Term.Num(1))),
        Term.Bool(true),
        Term.Apply("mul", List(Term.Num(2), Term.Num(3)))
      )
    )

    val reuslt   = TypeChecker.check(term, env)
    val expected = Type.Num

    reuslt shouldEqual expected

  }

}
