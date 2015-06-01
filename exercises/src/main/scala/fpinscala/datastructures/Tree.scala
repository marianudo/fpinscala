package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = {
    def f(t: Tree[A], acc: Int): Int = t match {
      case Leaf(_) => acc + 1
      case Branch(l, r) => f(l, acc) + f(r, acc)
    }
    f(t, 0)
  }
}
