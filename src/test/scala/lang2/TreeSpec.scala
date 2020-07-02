package lang2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TreeSpec extends AnyFlatSpec with Matchers {

  "Tree" should "reverse 1" in {

    // (1, (2, 3), (4, (5, 6)))
    val tree: Tree[Int] =
      Node(Leaf(1), Node(Leaf(2), Leaf(3)), Node(Leaf(4), Node(Leaf(5), Leaf(6))))
    val reversed = tree.reverse
    val expected = Node(Node(Node(Leaf(6), Leaf(5)), Leaf(4)), Node(Leaf(3), Leaf(2)), Leaf(1))

    reversed shouldEqual expected

  }

  "Tree" should "compact 1" in {
    // ((1)) => 1
    val tree      = Node(Node(Leaf(1)))
    val compacted = tree.compact
    val expected  = Leaf(1)

    compacted shouldEqual expected

  }

  "Tree" should "compact 2" in {
    // (1 ((2))) => (1 2)
    val tree      = Node(Leaf(1), Node(Node(Leaf(2))))
    val compacted = tree.compact
    val expected  = Node(Leaf(1), Leaf(2))

    compacted shouldEqual expected

  }

  "Tree" should "compact 3" in {
    // (((c, b), a), ()) => ((c, b), a)
    val tree: Tree[String] =
      Node(
        Node(
          Node(
            Leaf("c"),
            Leaf("b")
          ),
          Node(
            Leaf("a")
          )
        ),
        Node(Nil)
      )
    val compacted = tree.compact
    val expected  = Node(List(Node(List(Leaf("c"), Leaf("b"))), Leaf("a")))

    compacted shouldEqual expected
  }

  "Node" should "::: 1" in {
    val left     = new Node(List(Leaf(1), Leaf(2)))
    val right    = new Node(List(Leaf(3), Leaf(4)))
    val concat   = left ::: right
    val expected = new Node(List(Node(Leaf(1), Leaf(2)), Node(List(Leaf(3), Leaf(4)))))

    concat shouldEqual expected
  }

  "Node" should "::: 2" in {
    val left     = Node(Leaf(1))
    val right    = Node(Leaf(2), Leaf(3))
    val concat   = left ::: right
    val expected = Node(Node(Leaf(1)), Node(Leaf(2), Leaf(3)))

    concat shouldEqual expected
  }

}
