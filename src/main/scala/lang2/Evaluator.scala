package lang2
import lang2.Term.Lambda

object Evaluator {

  def eval(term: Term, env: Environment): Term.Const = evalEnv(term, env)._1

  def evalEnv(term: Term, env: Environment): (Term.Const, Environment) = {
    term match {
      case t: Term.Const => (t, env)
      case Term.Var(name) => {
        env.lookupVariable(name) match {
          case Some(term) => evalEnv(term, env)
          case None       => throw new VariableNotFound(name)
        }
      }
      case Term.Function(functionName, args) => {
        env.lookupFunction(functionName) match {
          case Some(func) => {
            val consts = args.map(eval(_, env))
            (func.apply(consts), env)
          }
          case None =>
            env.lookupLambda(functionName) match {
              case Some(lambda) => {
                val consts             = args.map(eval(_, env))
                val replacedTerm: Term = replace(lambda.term, lambda.argName, consts.head)
                evalEnv(replacedTerm, env)
              }
              case _ => throw new FuntionNotFound(functionName)
            }
        }
      }
      case Term.Let(variableName, init, term) => {
        val constInit = evalEnv(init, env)._1
        val newEnv = constInit match {
          case lambda: Lambda => env.scoped.withLambda(variableName, lambda)
          case _              => env.scoped.withVariable(variableName, constInit)
        }
        (evalEnv(term, newEnv)._1, env)
      }
    }
  }

  class VariableNotFound(variableName: String) extends Lang2Error(s"VariableNotFound: $variableName")
  class FuntionNotFound(functionName: String)  extends Lang2Error(s"FunctionNotFound: $functionName")

  def replace(term: Term, fromVarName: String, to: Term.Const): Term = {
    def loop(term: Term): Term = {
      term match {
        case Term.Function(fn, args)               => Term.Function(fn, args.map(loop(_)))
        case Term.Var(name) if name == fromVarName => to
        case Term.Let(vn, init, term)              => Term.Let(vn, loop(term), loop(term))
        case Term.Lambda(an, term)                 => Term.Lambda(an, loop(term))
        case other                                 => other
      }
    }
    loop(term)
  }

}
