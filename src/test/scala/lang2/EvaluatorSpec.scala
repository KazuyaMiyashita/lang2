package lang2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import Term._

class EvaluatorSpec extends AnyFlatSpec with Matchers {

  it should "evaluator 1" in {

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
    val reuslt   = Evaluator.eval(term, env)
    val expected = Num(7)

    reuslt shouldEqual expected

  }

  it should "evaluator 2" in {

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

    Evaluator.eval(term, env) shouldEqual Num(1)

  }

  it should "evaluator 3" in {

    val env = Environment(
      Map(
        "ifnum" -> Func.ifnum
      )
    )

    val term = Apply(
      "ifnum",
      List(
        Bool(false),
        Num(1),
        Bool(false)
      )
    )

    // ifnum はNumのみを返すはずだが、ここでは型のチェックを行わないのでBoolが返る
    Evaluator.eval(term, env) shouldEqual Bool(false)

  }

  it should "evaluator 4" in {

    val env = Environment(
      Map(
        "mul" -> Func.mul
      )
    )

    val term = Block(
      List(
        Num(1),
        Bool(true),
        Apply("mul", List(Num(2), Num(3)))
      )
    )

    Evaluator.eval(term, env) shouldEqual Num(6)

  }

}
