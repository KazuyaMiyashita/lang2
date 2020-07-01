package lang2

sealed trait Type {
  def parent: Option[Type]
  def asString: String
  def ->:(that: Type): Type.Function = Type.Function(that, this)
}

object Type {

  case object Any extends Type {
    override val parent   = None
    override val asString = "Any"
  }
  case object Bool extends Type {
    override val parent   = Some(Type.Any)
    override val asString = "Bool"
  }
  case object Num extends Type {
    override val parent   = Some(Type.Any)
    override val asString = "Num"
  }
  case class Function(a: Type, b: Type) extends Type {
    override val parent   = Some(Type.Any)
    override val asString = "(" + a.asString + " -> " + b.asString + ")"
  }
  case object Unit extends Type {
    override val parent   = Some(Type.Any)
    override val asString = "Unit"
  }
  case object Nothing extends Type {
    override val parent   = Some(Type.Any)
    override val asString = "Nothing"
  }

  def listToFunctionType(list: List[Term.Const]): Type = {
    list match {
      case Nil          => Type.Unit
      case head :: Nil  => head.tpe
      case head :: next => Type.Function(head.tpe, listToFunctionType(next))
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
    getHierarchy(t1).intersect(getHierarchy(t2)).headOption.getOrElse(Type.Any)
  }

}
