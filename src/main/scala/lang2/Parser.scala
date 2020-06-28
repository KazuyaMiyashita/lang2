package lang2

import Token._
import Term._

object Parser {

  def parse(tokens: List[Token]): Option[Term] = {
    def loop(parenIndex: Int, remaining: List[Token], terms: List[Term]): (Int, List[Token], List[Term]) = {
      (parenIndex, remaining, terms) match {
        case (n, _, _) if n < 0 => throw new Exception
        case (_, LParen :: tail, _) => {
          val (i, r, args) = loop(parenIndex + 1, tail, Nil)
          (i, r, terms ::: args)
        }
        case (_, RParen :: tail, _)     => (parenIndex - 1, tail, terms.reverse)
        case (_, Numeric(v) :: tail, _) => loop(parenIndex, tail, Const(v) :: terms)
        case (_, Command(v) :: tail, _) => {
          val (i, r, args) = loop(parenIndex, tail, Nil)
          loop(i, r, Apply(v, args) :: terms)
        }
        case (i, Nil, terms) => (i, Nil, terms.reverse)
      }
    }

    loop(0, tokens, Nil)._3.headOption
  }

}
