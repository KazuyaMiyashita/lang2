import lang2._
import scala.util.control.NonFatal

object Main extends App {

  val env = Environment(
    Map(
      "add" -> Func.add,
      "mul" -> Func.mul
    )
  )

  def loop(): Unit = {

    print("> ")
    val input = io.StdIn.readLine()
    if (input == ":exit") ()
    else {
      var tokens: List[Token]   = null
      var term: Term            = null
      var evaluated: Term.Const = null
      try {
        tokens = Tokenizer.tokenize(input)
        term = Parser.parse(tokens).get
        evaluated = Evaluator.eval(term, env)
        println(s"result: ${evaluated.value}")
      } catch {
        case NonFatal(e) => {
          val message =
            if (input eq null) "* input read error *"
            else if (tokens eq null) "* tokenize error *"
            else if (term eq null) "* parse error *"
            else if (evaluated eq null) "* evaluation error *"
            else ""
          println(message)
          e.printStackTrace()
          println(s"input: $input")
          println(s"tokens: $tokens")
          println(s"term: $term")
          println(s"evaluated: $evaluated")
        }
      } finally {
        loop()
      }
    }

  }

  println("*** LANG2 CONSOLE***")
  println("enter a formula. For example (add 1 (mul 2 3)) . The reuslt is 7.")
  println("Built-in functions are `add` and `mul`.")
  println("enter :exit to exit this console.")
  println()

  loop()

}
