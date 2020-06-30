package lang2

sealed trait Term
sealed trait Const extends Term {
  def value: Any
  def tpe: Type
}

object Term {
  case class Bool(value: Boolean) extends Const {
    override def tpe = Type.BoolType
  }
  case class Num(value: Int) extends Const {
    override def tpe = Type.NumType
  }
  case object Uni extends Const {
    override val value = ()
    override def tpe   = Type.UnitType
  }
  case class Apply(functionName: String, args: List[Term]) extends Term
  case class Block(terms: List[Term])                      extends Term
}
