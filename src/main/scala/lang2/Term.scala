package lang2

sealed trait Term

object Term {
  sealed trait Const extends Term {
    def value: Any
    def tpe: Type
  }

  case class Bool(value: Boolean) extends Const {
    override def tpe = Type.Bool
  }
  case class Num(value: Int) extends Const {
    override def tpe = Type.Num
  }
  case object Unit extends Const {
    override val value = ()
    override def tpe   = Type.Unit
  }
  case class Apply(functionName: String, args: List[Term])      extends Term
  case class Var(name: String)                                  extends Term
  case class Let(variableName: String, init: Term, block: Term) extends Term
  case class Lambda(argName: String, term: Term)                extends Term
  case class Block(terms: List[Term])                           extends Term
}
