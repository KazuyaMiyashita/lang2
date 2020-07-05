package lang2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Lang2Spec extends AnyFlatSpec with Matchers {

  def eval(input: String, env: Environment): Term.Const = {
    val tokens = Tokenizer.tokenize(input)
    val term   = Parser.parse(tokens)
    Evaluator.eval(term, env)
  }

  it should "let f (lambda a a) (f 1)" in {
    val env = Environment.empty.withFunction("add", Function.add)
    eval("let f (lambda a a) (f 1)", env) shouldEqual Term.Num(1)
  }

  it should "(lambda a a) 1" in {
    val env = Environment.empty.withFunction("add", Function.add)
    eval("(lambda a a) 1", env) shouldEqual Term.Num(1)
  }

  it should "add 1 2" in {
    val env = Environment.empty.withFunction("add", Function.add)
    eval("add 1 2", env) shouldEqual Term.Num(3)
  }

  it should "let f add (f 1 2)" in {
    val env = Environment.empty.withFunction("add", Function.add)
    eval("let f add (f 1 2)", env) shouldEqual Term.Num(3)
  }

  it should "do lazy evaluation (ifnum true 1 error)" in {
    val env = Environment.empty
      .withFunction("ifnum", Function.ifnum)
      .withFunction("error", Function.error)
    eval("ifnum true 1 error", env) shouldEqual Term.Num(1)
  }

}
