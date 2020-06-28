package lang2

import Term.Const

trait Func {
  def apply(args: List[Const]): Const
}

object Func {

  val add: Func = { args: List[Const] =>
    if (args.length != 2) throw new IllegalArgumentException
    else Const(args(0).value + args(1).value)
  }

  val mul: Func = { args: List[Const] =>
    if (args.length != 2) throw new IllegalArgumentException
    else Const(args(0).value * args(1).value)
  }

}
