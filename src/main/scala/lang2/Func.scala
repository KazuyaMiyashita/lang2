package lang2

import Term._
import Type._

trait Func {
  def name: String
  def apply(args: List[Const]): Const
  def tpe: FunctionType
  def argsType: List[Type]
  def resultType: Type
}

object Func {

  val add: Func = new Func {
    override def name: String = "add"
    override def apply(args: List[Const]): Const = {
      args match {
        case Num(n1) :: Num(n2) :: Nil => Num(n1 + n2)
        case _                         => throw new IllegalArgumentException
      }
    }

    override def tpe                  = FunctionType(NumType, NumType)
    override def argsType: List[Type] = NumType :: NumType :: Nil
    override def resultType: Type     = NumType
  }

  val mul: Func = new Func {
    override def name: String = "mul"
    override def apply(args: List[Const]): Const = {
      args match {
        case Num(n1) :: Num(n2) :: Nil => Num(n1 * n2)
        case _                         => throw new IllegalArgumentException
      }
    }

    override def tpe                  = FunctionType(NumType, NumType)
    override def argsType: List[Type] = NumType :: NumType :: Nil
    override def resultType: Type     = NumType
  }

  val ifnum: Func = new Func {
    override def name: String = "ifnum"
    override def apply(args: List[Const]): Const = {
      args match {
        case Bool(cond) :: (a2: Const) :: (a3: Const) :: Nil => if (cond) a2 else a3
        case _                                               => throw new IllegalArgumentException
      }
    }

    override def tpe = {
      FunctionType(BoolType, FunctionType(NumType, NumType))
    }
    override def argsType: List[Type] = BoolType :: NumType :: NumType :: Nil
    override def resultType: Type     = NumType
  }

}
