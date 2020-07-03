package lang2

sealed trait Tree[A] {
  import Tree.{Node, Leaf}

  def reverse: Tree[A] = this match {
    case Node(children) => Node(children.map(_.reverse).reverse)
    case leaf: Leaf[A]  => leaf
  }

  def compact: Tree[A] = this match {
    case Node(children) =>
      children.filterNot(_ == Node(Nil)) match {
        case Nil      => Node(Nil)
        case c :: Nil => c.compact
        case _        => Node(children.map(_.compact))
      }
    case other => other
  }
}

object Tree {

  case class Node[A](children: List[Tree[A]]) extends Tree[A]
  case class Leaf[A](value: A)                extends Tree[A]

  object Node {
    def apply[A](children: Tree[A]*) = new Node(children.toList)
    def empty[A]                     = new Node[A](Nil)
  }

  def makeTree[A](list: List[A], lp: A, rp: A): Tree[A] = {
    def loop(
        parenLevel: Int,
        remaining: List[A],
        acc: Node[A]
    ): (Int, List[A], Node[A]) = {
      remaining match {
        case `lp` :: tail => {
          val (i, r, t) = loop(parenLevel + 1, tail, Node.empty[A])
          (i, r, new Node(t.children ::: acc.children))
        }
        case `rp` :: tail => {
          if (parenLevel < 0) throw new ParseTreeError("Unexpected brackets")
          else loop(parenLevel - 1, tail, new Node(acc :: Nil))
        }
        case elem :: tail => {
          loop(parenLevel, tail, new Node(Leaf(elem) :: acc.children))
        }
        case Nil => (parenLevel, Nil, acc)
      }
    }
    loop(0, list, Node.empty[A])._3.compact.reverse
  }

  class ParseTreeError(message: String) extends Lang2Error("ParseError: " + message)

}
