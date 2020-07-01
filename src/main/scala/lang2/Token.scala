package lang2

sealed trait Token
object Token {
  case object LParen              extends Token
  case object RParen              extends Token
  sealed trait Keyword            extends Token
  case object Let                 extends Keyword
  case object Lambda              extends Keyword
  case class Word(value: String)  extends Token
  case class Num(value: Int)      extends Token
  case class Bool(value: Boolean) extends Token
}
