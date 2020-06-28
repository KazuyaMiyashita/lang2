package lang2

import Token._

object Tokenizer {
  private def handleCommand(remaining: List[Char], acc: List[Token]): (List[Char], List[Token]) = {
    def loop(remaining: List[Char], chars: List[Char]): (List[Char], List[Char]) = {
      remaining match {
        case head :: tail if ('a' to 'z').contains(head) => loop(tail, head :: chars)
        case _                                           => (remaining, chars)
      }
    }
    val (newRemaing, newAcc) = loop(remaining, Nil)
    (newRemaing, Command(newAcc.reverse.mkString) :: acc)
  }
  private def handleNumeric(remaining: List[Char], acc: List[Token]): (List[Char], List[Token]) = {
    def loop(remaining: List[Char], chars: List[Char]): (List[Char], List[Char]) = {
      remaining match {
        case head :: tail if ('0' to '9').contains(head) => loop(tail, head :: chars)
        case _                                           => (remaining, chars)
      }
    }
    val (newRemaing, newAcc) = loop(remaining, Nil)
    (newRemaing, Numeric(newAcc.reverse.mkString.toInt) :: acc)
  }
  def tokenize(input: String): List[Token] = {
    def loop(remaining: List[Char], acc: List[Token]): (List[Char], List[Token]) = {
      remaining match {
        case ' ' :: tail => loop(tail, acc)
        case '(' :: tail => loop(tail, LParen :: acc)
        case ')' :: tail => loop(tail, RParen :: acc)
        case head :: _ if ('a' to 'z').contains(head) => {
          val (newRemaing, newAcc) = handleCommand(remaining, acc)
          loop(newRemaing, newAcc)
        }
        case head :: _ if ('0' to '9').contains(head) => {
          val (newRemaing, newAcc) = handleNumeric(remaining, acc)
          loop(newRemaing, newAcc)
        }
        case Nil => (remaining, acc)
        case _   => throw new Exception("不正な文字が含まれています")
      }
    }
    loop(input.toList, Nil)._2.reverse
  }
}
