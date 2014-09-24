package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] =
    foldRight(List.empty[A])((a, l) => a :: l)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
      case _ => Stream.empty[A]
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
      case _ => Stream.empty[A]
    }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) => if (p(h)) cons(h, t.takeWhile2(p)) else Stream.empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((h, t) => p(h) && t)

  def startsWith[B](s: Stream[B]): Boolean =
    (this, s) match {
      case (_, Empty)          => true
      case (Empty, Cons(_, _)) => false
      case (Cons(h1, t1), Cons(h2, t2)) => (h1() == h2()) && t1().startsWith(t2())
    }

  // exercise 5.6
  def headOption: Option[A] =
    foldRight(Option.empty[A])((h, _) => Some(h))

  // exercise 5.7
  
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[AA >: A](s: => Stream[AA]): Stream[AA] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((h, t) => f(h).append(t))

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

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))
    
  def fibs: Stream[Int] = ???
    //0 1 1 2 3 5

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
  
  // fibs, from, constant, ones
  
  // map, take, takeWhile, zipWith, zipAll
}
