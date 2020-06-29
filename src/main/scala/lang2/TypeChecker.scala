package lang2
import lang2.Term.Apply

object TypeChecker {

  def check(term: Term, env: Environment): Type = {
    term match {
      case t: Const => t.tpe
      case Apply(functionName, args) => {
        env.functions.get(functionName) match {
          case None => throw new RuntimeException(s"function $functionName not found")
          case Some(func) => {
            val argsType = args.map(check(_, env))
            func.tpe.applyTypes(argsType) match {
              case Some(nextType) => nextType
              case None           => throw new TypeChecker.IllegalArgumentTypeError(func, argsType)
            }
          }
        }
      }
    }
  }

  class IllegalArgumentTypeError(func: Func, foundTypes: List[Type])
      extends Exception(
        s"TypeError: function `${func.name}` requires arguments: ${func.argsType
          .map(_.asString)
          .mkString("(", ", ", ")")} , but found: ${foundTypes.map(_.asString).mkString("(", ", ", ")")}"
      )

}
