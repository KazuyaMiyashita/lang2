package lang2

sealed trait Type {
  def parent: Option[Type]
  def asString: String
}

object Type {

  case object AnyType extends Type {
    override val parent   = None
    override val asString = "Any"
  }
  case object BoolType extends Type {
    override val parent   = Some(AnyType)
    override val asString = "Bool"
  }
  case object NumType extends Type {
    override val parent   = Some(AnyType)
    override val asString = "Num"
  }
  case class FunctionType(a: Type, b: Type) extends Type {
    override val parent   = Some(AnyType)
    override val asString = "(" + a.asString + " -> " + b.asString + ")"

    def toList: List[Type] = {
      (a, b) match {
        case (_: Type, t: FunctionType) => a :: t.toList
        case (_, _)                     => a :: b :: Nil
      }
    }

    def applyTypes(args: List[Type]): Option[Type] = {
      if (args == toList) Some(toList.last)
      else None
    }
  }
  case object UnitType extends Type {
    override val parent   = Some(AnyType)
    override val asString = "Unit"
  }
  case object NothingType extends Type {
    override val parent   = Some(AnyType)
    override val asString = "Nothing"
  }

  def listToFunctionType(list: List[Const]): Type = {
    list match {
      case Nil          => UnitType
      case head :: Nil  => head.tpe
      case head :: next => FunctionType(head.tpe, listToFunctionType(next))
    }
  }

  def getHierarchy(tpe: Type): List[Type] = {
    def loop(t: Type, acc: List[Type]): (Type, List[Type]) = {
      t.parent match {
        case Some(p) => loop(p, p :: acc)
        case None    => (t, acc)
      }
    }
    loop(tpe, tpe :: Nil)._2.reverse
  }

  def upper(t1: Type, t2: Type): Type = {
    getHierarchy(t1).intersect(getHierarchy(t2)).headOption.getOrElse(AnyType)
  }

}
