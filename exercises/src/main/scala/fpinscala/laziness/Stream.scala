package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => { println(s"f($h, $t.fR($z)($f))"); f(h(), t().foldRight(z)(f))} // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) => if(n > 0) cons(h(), t().take(n - 1)) else empty
    case _ => this
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream[A]())((a, b) => if(p(a)) cons(a, b) else empty)

  def forAll[B](p: A => Boolean): Boolean =
    this.foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream[B]())((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else b)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def toList(): List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def from(n: Int): Stream[Int] =
    cons(n, from(n+1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    val frs = f(z)
    frs match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty[A]
    }
  }

  def toList[A](s: Stream[A]): List[A] = s match {
    case Empty => Nil
    case Cons(h, t) => h() :: toList[A](t())
  }

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] =
      cons(a, go(b, a+b))

    go(0, 1)
  }
}
