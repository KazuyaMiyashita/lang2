package lang2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import Tree.{Node, Leaf}

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

  "Tree#makeTree" should "a b c" in {
    val tokens   = "abc".toList
    val tree     = Tree.makeTree(tokens, '(', ')')
    val expected = Node(Leaf('a'), Leaf('b'), Leaf('c'))

    tree shouldEqual expected
  }

  "Tree#makeTree" should "((a) (b) (c))" in {
    val tokens   = "((a)(b)(c))".toList
    val tree     = Tree.makeTree(tokens, '(', ')')
    val expected = Node(Leaf('a'), Leaf('b'), Leaf('c'))

    tree shouldEqual expected
  }

  "Tree#makeTree" should "(a (b c))" in {
    val tokens   = "(a(bc))".toList
    val tree     = Tree.makeTree(tokens, '(', ')')
    val expected = Node(Leaf('a'), Node(Leaf('b'), Leaf('c')))

    tree shouldEqual expected
  }

  "Tree#makeTree" should "(a b (c d))" in {
    val tokens = "(ab(cd))".toList
    val tree   = Tree.makeTree(tokens, '(', ')')
    val expected = Node(
      Leaf('a'),
      Leaf('b'),
      Node(
        Leaf('c'),
        Leaf('d')
      )
    )
    tree shouldEqual expected
  }

  // ここから落ちる
  "Tree#makeTree" should "(a (b (c d))" in {
    val tokens = "(a(b(cd)))".toList
    val tree   = Tree.makeTree(tokens, '(', ')')
    val expected = Node(
      Leaf('a'),
      Node(
        Leaf('b'),
        Node(
          Leaf('c'),
          Leaf('d')
        )
      )
    )
    tree shouldEqual expected
  }

  "Tree#makeTree" should "(a b (c d (e f g)))" in {
    val tokens = "(ab(cd(efg)))".toList
    val tree   = Tree.makeTree(tokens, '(', ')')
    val expected = Node(
      Leaf('a'),
      Leaf('b'),
      Node(
        Leaf('c'),
        Leaf('d'),
        Node(
          Leaf('e'),
          Leaf('f'),
          Leaf('g')
        )
      )
    )

    tree shouldEqual expected
  }

  "Tree#makeTree" should "(let f (lambda x (add n x))" in {
    val tokens =
      "(" :: "let" :: "f" :: "(" :: "lambda" :: "x" :: "(" :: "add" :: "n" :: "x" :: ")" :: ")" :: Nil
    val tree = Tree.makeTree(tokens, "(", ")")
    val expected =
      Node(
        Leaf("let"),
        Leaf("f"),
        Node(
          Leaf("lambda"),
          Leaf("x"),
          Node(
            Leaf("add"),
            Leaf("n"),
            Leaf("x")
          )
        )
      )

    tree shouldEqual expected
  }

  "Tree#makeTree" should "(let n 1 (let f (lambda x (add n x)) (f 2)))" in {
    val tokens =
      "(" :: "let" :: "n" :: "1" :: "(" :: "let" :: "f" :: "(" :: "lambda" :: "x" :: "(" :: "add" :: "n" :: "x" :: ")" :: ")" :: "(" :: "f" :: "2" :: ")" :: ")" :: ")" :: Nil
    val tree = Tree.makeTree(tokens, "(", ")")
    val expected = Node(
      Leaf("let"),
      Leaf("n"),
      Leaf("1"),
      Node(
        Leaf("let"),
        Leaf("f"),
        Node(
          Leaf("lambda"),
          Leaf("x"),
          Node(
            Leaf("add"),
            Leaf("n"),
            Leaf("x")
          )
        ),
        Node(
          Leaf("f"),
          Leaf("2")
        )
      )
    )

    tree shouldEqual expected
  }

}
