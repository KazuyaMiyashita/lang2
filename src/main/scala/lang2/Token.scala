package lang2

sealed trait Token
object Token {
  case object LParen                extends Token
  case object RParen                extends Token
  case class Command(value: String) extends Token
  case class Numeric(value: Int)    extends Token
  case class Boole(value: Boolean)  extends Token
}
