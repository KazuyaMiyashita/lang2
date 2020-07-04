package lang2

import scala.collection.mutable

case class Environment(
    parent: Option[Environment],
    private val variables: mutable.Map[String, Term],
    private val functions: mutable.Map[String, Function]
) {

  def lookup(variableName: String): Option[Term] = {
    variables.get(variableName).orElse(parent.flatMap(_.lookup(variableName)))
  }

  def register(variableName: String, term: Term): Unit = {
    variables.put(variableName, term)
    ()
  }

  def lookupFunction(functionName: String): Option[Function] = {
    functions.get(functionName).orElse(parent.flatMap(_.lookupFunction(functionName)))
  }

  def registerFunction(functionName: String, func: Function): Unit = {
    functions.put(functionName, func)
    ()
  }

  def scope: Environment = Environment(Some(this), mutable.Map.empty, mutable.Map.empty)

}
