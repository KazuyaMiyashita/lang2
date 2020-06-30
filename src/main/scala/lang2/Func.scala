package lang2

import Term._
import Type._

trait Func {
  def name: String
  def tpe: FunctionType
  def apply(args: List[Const]): Const

  private def tpeToList(ft: FunctionType): List[Type] = {
    (ft.a, ft.b) match {
      case (_: Type, t: FunctionType) => ft.a :: tpeToList(t)
      case (_, _)                     => ft.a :: ft.b :: Nil
    }
  }
  final def argsType: List[Type] = tpeToList(tpe).init
  final def resultType: Type     = tpeToList(tpe).last

  final def matchesArgsType(args: List[Type]): Option[Type] = {
    def applyTypes(ft: FunctionType, args: List[Type]): Option[Type] = {
      args match {
        case Nil => Some(ft)
        case head :: tail =>
          applyType(ft, head) match {
            case Some(next: FunctionType)   => applyTypes(next, tail)
            case Some(next) if tail.isEmpty => Some(next)
            case _                          => None
          }
      }
    }

    def applyType(ft: FunctionType, thatType: Type): Option[Type] = {
      if (ft.a == thatType) Some(ft.b)
      else None
    }

    applyTypes(tpe, args)
  }
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

    override def tpe = NumType ->: NumType ->: NumType
  }

  val mul: Func = new Func {
    override def name: String = "mul"
    override def apply(args: List[Const]): Const = {
      args match {
        case Num(n1) :: Num(n2) :: Nil => Num(n1 * n2)
        case _                         => throw new IllegalArgumentException
      }
    }

    override def tpe = NumType ->: NumType ->: NumType
  }

  val ifnum: Func = new Func {
    override def name: String = "ifnum"
    override def apply(args: List[Const]): Const = {
      args match {
        case Bool(cond) :: (a2: Const) :: (a3: Const) :: Nil => if (cond) a2 else a3
        case _                                               => throw new IllegalArgumentException
      }
    }

    override def tpe = BoolType ->: NumType ->: NumType ->: NumType
  }

  val echonum: Func = new Func {
    override def name: String = "echonum"
    override def apply(args: List[Const]): Const = {
      args match {
        case Num(n1) :: Nil => {
          println(n1)
          Uni
        }
        case _ => throw new IllegalArgumentException
      }
    }

    override def tpe = NumType ->: NumType
  }

}
