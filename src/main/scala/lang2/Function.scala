package lang2

trait Function {
  def apply(args: List[Term.Const]): Term.Const
}

object Function {

  val add: Function = new Function {
    override def apply(args: List[Term.Const]): Term.Const = {
      args match {
        case Term.Num(n1) :: Term.Num(n2) :: Nil => Term.Num(n1 + n2)
        case _                                   => throw new IllegalArgumentException
      }
    }
  }

  val mul: Function = new Function {
    override def apply(args: List[Term.Const]): Term.Const = {
      args match {
        case Term.Num(n1) :: Term.Num(n2) :: Nil => Term.Num(n1 * n2)
        case _                                   => throw new IllegalArgumentException
      }
    }
  }

  val ifnum: Function = new Function {
    override def apply(args: List[Term.Const]): Term.Const = {
      args match {
        case Term.Bool(cond) :: (a2: Term.Const) :: (a3: Term.Const) :: Nil => if (cond) a2 else a3
        case _                                                              => throw new IllegalArgumentException
      }
    }
  }

  val echonum: Function = new Function {
    override def apply(args: List[Term.Const]): Term.Const = {
      args match {
        case Term.Num(n1) :: Nil => {
          println(n1)
          Term.Unit
        }
        case _ => throw new IllegalArgumentException
      }
    }
  }

  def createNumFunction(argName: String, term: Term): Function = new Function {
    override def apply(args: List[Term.Const]): Term.Const = {
      val arg = args.head
      def replace(term: Term): Term = {
        term match {
          case Term.Function(fn, args)           => Term.Function(fn, args.map(replace))
          case Term.Var(name) if name == argName => arg
          case Term.Let(vn, init, term)          => Term.Let(vn, replace(term), replace(term))
          case Term.Lambda(an, term)             => Term.Lambda(an, replace(term))
          case other                             => other
        }
      }
      replace(term) match {
        case n1: Term.Num => n1
        case _            => throw new IllegalArgumentException
      }
    }
  }

}
