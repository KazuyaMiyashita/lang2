package lang2

import scala.collection.immutable

case class Environment(
    parent: Option[Environment],
    private val variables: immutable.Map[String, Term.Const],
    private val lambdas: immutable.Map[String, Term.Lambda],
    private val functions: immutable.Map[String, Function]
) {

  def lookupVariable(variableName: String): Option[Term.Const] = {
    variables.get(variableName).orElse(parent.flatMap(_.lookupVariable(variableName)))
  }

  def withVariable(variableName: String, term: Term.Const): Environment = {
    copy(variables = variables.updated(variableName, term))
  }

  def lookupLambda(name: String): Option[Term.Lambda] = {
    lambdas.get(name).orElse(parent.flatMap(_.lookupLambda(name)))
  }

  def withLambda(name: String, term: Term.Lambda): Environment = {
    copy(lambdas = lambdas.updated(name, term))
  }

  def lookupFunction(functionName: String): Option[Function] = {
    functions.get(functionName).orElse(parent.flatMap(_.lookupFunction(functionName)))
  }

  def withFunction(functionName: String, function: Function): Environment = {
    copy(functions = functions.updated(functionName, function))
  }

  def scoped: Environment = Environment(Some(this), immutable.Map.empty, immutable.Map.empty, immutable.Map.empty)

}

object Environment {

  def empty: Environment = Environment(None, immutable.Map.empty, immutable.Map.empty, immutable.Map.empty)

}
