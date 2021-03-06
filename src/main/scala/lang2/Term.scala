package lang2

sealed trait Term

object Term {
  sealed trait Const extends Term {
    def value: Any
  }

  case class Bool(value: Boolean) extends Const
  case class Num(value: Int)      extends Const
  case object Unit extends Const {
    override val value = ()
  }
  case class Lambda(argName: String, term: Term) extends Const {
    def value = "(lambda)"
  }
  case class Function(functionName: String, args: List[Term])  extends Term
  case class Var(name: String)                                 extends Term
  case class Let(variableName: String, init: Term, term: Term) extends Term
}
