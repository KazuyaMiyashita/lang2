package lang2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Lang2Spec extends AnyFlatSpec with Matchers {

  val env = Environment.empty
    .withFunction("add", Function.add)

  def eval(input: String): Term.Const = {
    val tokens = Tokenizer.tokenize(input)
    val term   = Parser.parse(tokens)
    Evaluator.eval(term, env)
  }

  it should "let f (lambda a a) (f 1)" in {
    eval("let f (lambda a a) (f 1)") shouldEqual Term.Num(1)
  }

  it should "(lambda a a) 1" in {
    eval("(lambda a a) 1") shouldEqual Term.Num(1)
  }

  it should "add 1 2" in {
    eval("add 1 2") shouldEqual Term.Num(3)
  }

  it should "let f add (f 1 2)" in {
    eval("let f add (f 1 2)") shouldEqual Term.Num(3)
  }

}
