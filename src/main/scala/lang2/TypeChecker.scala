package lang2

object TypeChecker {

  def check(term: Term, env: Environment): Type = {
    checkEnv(term, env)._1
  }

  def checkEnv(term: Term, env: Environment): (Type, Environment) = {
    term match {
      case t: Term.Const => (t.tpe, env)
      case Term.Function(functionName, args) => {
        env.functions.get(functionName) match {
          case None => throw new RuntimeException(s"function $functionName not found")
          case Some(func) => {
            val argsType = args.map(check(_, env))
            func.matchesArgsType(argsType) match {
              case Some(nextType) => (nextType, env)
              case None           => throw new TypeChecker.IllegalArgumentTypeError(func, argsType)
            }
          }
        }
      }
      case _ => ???
    }
  }

  class IllegalArgumentTypeError(func: Func, foundTypes: List[Type])
      extends Lang2Error(
        s"TypeError: function `${func.name}` requires arguments: ${func.argsType
          .map(_.asString)
          .mkString("(", ", ", ")")} , but found: ${foundTypes.map(_.asString).mkString("(", ", ", ")")}"
      )

}
