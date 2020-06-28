package lang2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import Term._

class EvaluatorSpec extends AnyFlatSpec with Matchers {

  val env = Environment(
    Map(
      "add" -> Func.add,
      "mul" -> Func.mul
    )
  )

  it should "parser 1" in {

    val term = Apply(
      "add",
      List(
        Const(1),
        Apply(
          "mul",
          List(
            Const(2),
            Const(3)
          )
        )
      )
    )
    val reuslt   = Evaluator.eval(term, env)
    val expected = Const(7)

    reuslt shouldEqual expected

  }

}
