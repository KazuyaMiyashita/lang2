package lang2

import Term._

object Evaluator {

  def eval(term: Term, env: Environment): Const = {
    evalEnv(term, env)._1
  }

  def evalEnv(term: Term, env: Environment): (Const, Environment) = {
    term match {
      case t: Const => (t, env)
      case Apply(functionName, args) => {
        env.functions.get(functionName) match {
          case None => throw new RuntimeException(s"function $functionName not found")
          case Some(func) => {
            val consts = args.map(eval(_, env))
            (func.apply(consts), env)
          }
        }
      }
      case Block(terms) => {
        terms.foldLeft(((Uni: Const), env))((te, term) => evalEnv(term, te._2))
      }
    }
  }

}
