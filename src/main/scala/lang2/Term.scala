package lang2

trait Term
object Term {
  case class Const(value: Int)                             extends Term
  case class Apply(functionName: String, args: List[Term]) extends Term
}
