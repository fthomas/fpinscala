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
  def check: Boolean

  def &&(p: Prop): Prop =
    new Prop {
      def check = this.check && p.check
    }
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] =
    Gen(State(RNG.unit(a)))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val range = stopExclusive - start
    val rng = RNG.map(RNG.nonNegativeLessThan(range))(_ + start)
    Gen(State(rng))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    for {
      a1 <- g1
      a2 <- g2
      b <- boolean
    } yield if (b) a1 else a2

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = ???
}

case class Gen[A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = ???

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(i => Gen.listOfN(i, this))
}

trait SGen[+A] {

}

