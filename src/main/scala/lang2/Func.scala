package lang2

trait Func {
  def name: String
  def tpe: Type.Function
  def apply(args: List[Term.Const]): Term.Const

  private def tpeToList(ft: Type.Function): List[Type] = {
    (ft.a, ft.b) match {
      case (_: Type, t: Type.Function) => ft.a :: tpeToList(t)
      case (_, _)                      => ft.a :: ft.b :: Nil
    }
  }
  final def argsType: List[Type] = tpeToList(tpe).init
  final def resultType: Type     = tpeToList(tpe).last

  final def matchesArgsType(args: List[Type]): Option[Type] = {
    def applyTypes(ft: Type.Function, args: List[Type]): Option[Type] = {
      args match {
        case Nil => Some(ft)
        case head :: tail =>
          applyType(ft, head) match {
            case Some(next: Type.Function)  => applyTypes(next, tail)
            case Some(next) if tail.isEmpty => Some(next)
            case _                          => None
          }
      }
    }

    def applyType(ft: Type.Function, thatType: Type): Option[Type] = {
      if (ft.a == thatType) Some(ft.b)
      else None
    }

    applyTypes(tpe, args)
  }
}

object Func {

  val add: Func = new Func {
    override def name: String = "add"
    override def apply(args: List[Term.Const]): Term.Const = {
      args match {
        case Term.Num(n1) :: Term.Num(n2) :: Nil => Term.Num(n1 + n2)
        case _                                   => throw new IllegalArgumentException
      }
    }

    override def tpe = Type.Num ->: Type.Num ->: Type.Num
  }

  val mul: Func = new Func {
    override def name: String = "mul"
    override def apply(args: List[Term.Const]): Term.Const = {
      args match {
        case Term.Num(n1) :: Term.Num(n2) :: Nil => Term.Num(n1 * n2)
        case _                                   => throw new IllegalArgumentException
      }
    }

    override def tpe = Type.Num ->: Type.Num ->: Type.Num
  }

  val ifnum: Func = new Func {
    override def name: String = "ifnum"
    override def apply(args: List[Term.Const]): Term.Const = {
      args match {
        case Term.Bool(cond) :: (a2: Term.Const) :: (a3: Term.Const) :: Nil => if (cond) a2 else a3
        case _                                                              => throw new IllegalArgumentException
      }
    }

    override def tpe = Type.Bool ->: Type.Num ->: Type.Num ->: Type.Num
  }

  val echonum: Func = new Func {
    override def name: String = "echonum"
    override def apply(args: List[Term.Const]): Term.Const = {
      args match {
        case Term.Num(n1) :: Nil => {
          println(n1)
          Term.Unit
        }
        case _ => throw new IllegalArgumentException
      }
    }

    override def tpe = Type.Num ->: Type.Unit
  }

}
