package lang2

import Term._
import Type._

trait Func {
  def apply(args: List[Const]): Const
  def tpe: Type
}

object Func {

  val add: Func = new Func {
    override def apply(args: List[Const]): Const = {
      args match {
        case Num(n1) :: Num(n2) :: Nil => Num(n1 + n2)
        case _                         => throw new IllegalArgumentException
      }
    }

    override def tpe = FunctionType(NumType, NumType)
  }

  val mul: Func = new Func {
    override def apply(args: List[Const]): Const = {
      args match {
        case Num(n1) :: Num(n2) :: Nil => Num(n1 * n2)
        case _                         => throw new IllegalArgumentException
      }
    }

    override def tpe = FunctionType(NumType, NumType)
  }

  val ifnum: Func = new Func {
    override def apply(args: List[Const]): Const = {
      args match {
        case Bool(cond) :: (a2: Const) :: (a3: Const) :: Nil => if (cond) a2 else a3
        case _                                               => throw new IllegalArgumentException
      }
    }

    override def tpe = {
      FunctionType(BoolType, FunctionType(NumType, NumType))
    }
  }

}
