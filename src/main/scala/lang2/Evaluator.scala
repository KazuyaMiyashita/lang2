package lang2

object Evaluator {

  def eval(term: Term, env: Environment): Term.Const = {
    println()
    evalEnv(term, env)._1
  }

  def evalEnv(term: Term, env: Environment): (Term.Const, Environment) = {
    println(s"term: $term, env: $env")
    term match {
      case t: Term.Const => (t, env)
      case Term.Var(name) => {
        env.lookup(name) match {
          case None       => throw new VariableNotFound(name)
          case Some(term) => evalEnv(term, env)
        }
      }
      case Term.Function(functionName, args) => {
        env.lookupFunction(functionName) match {
          case Some(func) => {
            val consts = args.map(eval(_, env))
            (func.apply(consts), env)
          }
          case None =>
            env.lookup(functionName) match {
              case Some(variable: Term.Const) => (variable, env)
              case _                          => throw new FuntionNotFound(functionName)
            }
        }
      }
      case Term.Let(variableName, init, term) => {
        val newEnv = env.scope
        newEnv.register(variableName, evalEnv(init, env)._1)
        (evalEnv(term, newEnv)._1, env)
      }
      case Term.Lambda(argName, term) => {
        val func: Function = Function.createNumFunction(argName, term)
        val newEnv         = env.scope
        newEnv.registerFunction(argName, func)
        (evalEnv(term, newEnv)._1, env)
      }
    }
  }

  class VariableNotFound(variableName: String) extends Lang2Error(s"VariableNotFound: $variableName")
  class FuntionNotFound(functionName: String)  extends Lang2Error(s"FunctionNotFound: $functionName")

}
