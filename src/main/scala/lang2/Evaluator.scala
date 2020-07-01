package lang2

object Evaluator {

  def eval(term: Term, env: Environment): Term.Const = {
    evalEnv(term, env)._1
  }

  def evalEnv(term: Term, env: Environment): (Term.Const, Environment) = {
    term match {
      case t: Term.Const => (t, env)
      case Term.Apply(functionName, args) => {
        env.functions.get(functionName) match {
          case None => throw new RuntimeException(s"function $functionName not found")
          case Some(func) => {
            val consts = args.map(eval(_, env))
            (func.apply(consts), env)
          }
        }
      }
      case Term.Block(terms) => {
        terms.foldLeft(((Term.Unit: Term.Const), env))((te, term) => evalEnv(term, te._2))
      }
    }
  }

}
