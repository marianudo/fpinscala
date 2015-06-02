package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  val treeFixture = Branch(
     Branch(
       Leaf(1),
       Leaf(2)
     ),
     Branch(
       Leaf(3),
       Leaf(6)
     )
  )

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
    case Leaf(v) => l(v)
    case Branch(ll, rr) => b(fold(ll)(l)(b), fold(rr)(l)(b))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_ + _ + 1)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(x => x)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((a, b) => a max b + 1)
}
