import lang2._
import scala.util.control.NonFatal

object Main extends App {

  val env = Environment(
    Map(
      "add"   -> Func.add,
      "mul"   -> Func.mul,
      "ifnum" -> Func.ifnum
    )
  )

  def loop(): Unit = {

    print("> ")
    val input = io.StdIn.readLine()
    if (input == ":exit") ()
    else {
      try {
        val tokens    = Tokenizer.tokenize(input)
        val term      = Parser.parse(tokens).get
        val tpe       = TypeChecker.check(term, env)
        val evaluated = Evaluator.eval(term, env)
        println(s"result: ${tpe.asString} = ${evaluated.value}")
      } catch {
        case NonFatal(e) => println(e.getMessage)
      } finally {
        println()
        loop()
      }
    }

  }

  println("*** LANG2 CONSOLE***")
  println("enter a formula. For example (add 1 (mul 2 3)) . The reuslt is 7.")
  println("Built-in functions are `add`, `mul`, `ifnum`.")
  println("enter :exit to exit this console.")
  println()

  loop()

}
