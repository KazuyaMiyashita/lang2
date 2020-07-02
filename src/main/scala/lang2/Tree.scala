package lang2

sealed trait Tree[A] {
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
case class Node[A](children: List[Tree[A]]) extends Tree[A] {
  def ::(leaf: Leaf[A]): Node[A]  = new Node(leaf :: children)
  def :::(node: Node[A]): Node[A] = new Node(List(node, this))
}
case class Leaf[A](value: A) extends Tree[A]

object Node {
  def apply[A](children: Tree[A]*) = new Node(children.toList)
  def empty[A]                     = new Node[A](Nil)
}
