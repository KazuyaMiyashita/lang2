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

  val error: Function = new Function {
    override def apply(args: List[Term.Const]): Term.Const = {
      throw new RuntimeException
    }
  }

}
