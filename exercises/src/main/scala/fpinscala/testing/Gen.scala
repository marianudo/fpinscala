package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def &&(p: Prop): Prop = new Prop {
    def check = Prop.this.check && p.check
  }
  def check: Boolean
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeInt) map (n => n + start % (stopExclusive - start)))
    // Alternative, baby steps based implementation (this is the one it's easier for me to come up with on my own)
    //val positiveIntState = State(RNG.nonNegativeInt)
    //val inRangeState = positiveIntState map (n => n + start % (stopExclusive - n))
    //Gen(inRangeState)
  }
}

case class Gen[A](sample: State[RNG,A]) {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

