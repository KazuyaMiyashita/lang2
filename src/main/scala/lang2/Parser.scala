package lang2

import Token._
import Term._

object Parser {

  def parse(tokens: List[Token]): Option[Term] = {
    def loop(parenIndex: Int, remaining: List[Token], terms: List[Term]): (Int, List[Token], List[Term]) = {
      (parenIndex, remaining) match {
        case (n, _) if n < 0 => throw new Exception
        case (_, LParen :: tail) => {
          val (i, r, args) = loop(parenIndex + 1, tail, Nil)
          (i, r, terms ::: args)
        }
        case (_, RParen :: tail)     => (parenIndex - 1, tail, terms.reverse)
        case (_, Numeric(v) :: tail) => loop(parenIndex, tail, Num(v) :: terms)
        case (_, Boole(v) :: tail)   => loop(parenIndex, tail, Bool(v) :: terms)
        case (_, Command(v) :: tail) => {
          val (i, r, args) = loop(parenIndex, tail, Nil)
          loop(i, r, Apply(v, args) :: terms)
        }
        case (i, Nil) => (i, Nil, terms.reverse)
      }
    }

    loop(0, tokens, Nil)._3.headOption
  }

}
