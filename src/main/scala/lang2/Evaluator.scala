package lang2

import Term._

object Evaluator {

  def eval(term: Term, env: Environment): Const = {
    term match {
      case t: Const => t
      case Apply(functionName, args) => {
        val consts = args.map(eval(_, env))
        env.functions.get(functionName) match {
          case None       => throw new RuntimeException(s"function $functionName not found")
          case Some(func) => func.apply(consts)
        }
      }
    }
  }

}
