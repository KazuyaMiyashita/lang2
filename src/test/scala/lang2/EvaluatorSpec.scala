package lang2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable

class EvaluatorSpec extends AnyFlatSpec with Matchers {

  it should "evaluator 1" in {

    val env = Environment(
      parent = None,
      variables = mutable.Map.empty,
      functions = mutable.Map(
        "add" -> Function.add,
        "mul" -> Function.mul
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
    val reuslt   = Evaluator.eval(term, env)
    val expected = Term.Num(7)

    reuslt shouldEqual expected

  }

  it should "evaluator 2" in {

    val env = Environment(
      parent = None,
      variables = mutable.Map.empty,
      functions = mutable.Map(
        "ifnum" -> Function.ifnum
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

    Evaluator.eval(term, env) shouldEqual Term.Num(1)

  }

  it should "evaluator 3" in {

    val env = Environment(
      parent = None,
      variables = mutable.Map.empty,
      functions = mutable.Map(
        "ifnum" -> Function.ifnum
      )
    )

    val term = Term.Function(
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
      parent = None,
      variables = mutable.Map.empty,
      functions = mutable.Map(
        "add" -> Function.add
      )
    )

    val term = Term.Let(
      "n",
      Term.Num(1),
      Term.Var("n")
    )

    Evaluator.eval(term, env) shouldEqual Term.Num(1)

  }

  it should "evaluator 5" in {

    val env = Environment(
      parent = None,
      variables = mutable.Map.empty,
      functions = mutable.Map.empty
    )

    val term = Term.Let(
      "f",
      Term.Lambda(
        "x",
        Term.Num(1)
      ),
      Term.Function("f", List(Term.Num(0)))
    )

    Evaluator.eval(term, env) shouldEqual Term.Num(1)

  }

  it should "evaluator 6" in {

    val env = Environment(
      parent = None,
      variables = mutable.Map.empty,
      functions = mutable.Map(
        "add" -> Function.add
      )
    )

    val term = Term.Let(
      "n",
      Term.Num(1),
      Term.Let(
        "f",
        Term.Lambda("x", Term.Function("add", List(Term.Var("n"), Term.Var("x")))),
        Term.Function("f", List(Term.Num(2)))
      )
    )

    Evaluator.eval(term, env) shouldEqual Term.Num(3)

  }

}
