package lang2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EvaluatorSpec extends AnyFlatSpec with Matchers {

  it should "evaluator 1" in {

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
    val reuslt   = Evaluator.eval(term, env)
    val expected = Term.Num(7)

    reuslt shouldEqual expected

  }

  it should "evaluator 2" in {

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

    Evaluator.eval(term, env) shouldEqual Term.Num(1)

  }

  it should "evaluator 3" in {

    val env = Environment(
      Map(
        "ifnum" -> Func.ifnum
      )
    )

    val term = Term.Apply(
      "ifnum",
      List(
        Term.Bool(false),
        Term.Num(1),
        Term.Bool(false)
      )
    )

    // ifnum はNumのみを返すはずだが、ここでは型のチェックを行わないのでBoolが返る
    Evaluator.eval(term, env) shouldEqual Term.Bool(false)

  }

  it should "evaluator 4" in {

    val env = Environment(
      Map(
        "mul" -> Func.mul
      )
    )

    val term = Term.Block(
      List(
        Term.Num(1),
        Term.Bool(true),
        Term.Apply("mul", List(Term.Num(2), Term.Num(3)))
      )
    )

    Evaluator.eval(term, env) shouldEqual Term.Num(6)

  }

}
