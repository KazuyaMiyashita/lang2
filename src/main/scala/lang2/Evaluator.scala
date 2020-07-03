package lang2

object Evaluator {

  def eval(term: Term, env: Environment): Term.Const = {
    evalEnv(term, env)._1
  }

  def evalEnv(term: Term, env: Environment): (Term.Const, Environment) = {
    term match {
      case t: Term.Const => (t, env)
      case Term.Function(functionName, args) => {
        env.functions.get(functionName) match {
          case None => throw new RuntimeException(s"function $functionName not found")
          case Some(func) => {
            val consts = args.map(eval(_, env))
            (func.apply(consts), env)
          }
        }
      }
      case _ => ???
    }
  }

}
