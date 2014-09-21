package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => throw new Exception
      case Cons(_, xs) => xs
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => throw new Exception
      case Cons(_, t) => Cons(h, t)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Cons(x, xs) if n > 0 => drop(xs, n - 1)
      case _ => l
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Cons(x, Nil) => Nil
      case Cons(x, xs)  => Cons(x, init(xs))
      case Nil          => throw new Exception
    }

  def length[A](l: List[A]): Int =
    l match {
      case Nil => 0
      case Cons(x, xs) => 1 + length(xs)
    }

  def length_2[A](l: List[A]): Int =
    foldLeft(l, 0)((x, _) => x + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  // exercise 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((a, b) => Cons(b, a))

  // exercise 3.13
  def foldRight2[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b =>  g(f(a, b)))(z)

  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  // exercise 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((a, b) => Cons(a, b))

  // exercise 3.15
  def flatten[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil: List[A])(append)

  // exercise 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }

  // exercise 3.19
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) => {
        val t = filter(xs)(f)
        if (f(x)) Cons(x, t) else t
      }
    }

  // exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))

  // exercise 3.21
  def filter2[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  // exercise 3.23
  def zipWith[A, B](as: List[A], bs: List[B]): List[(A, B)] =
    (as, bs) match {
      case (Cons(ah, at), Cons(bh, bt)) => Cons((ah, bh), zipWith(at, bt))
      case _ => Nil
    }

  // exercise 3.24

  def beginsWith[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match {
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => beginsWith(t1, t2)
      case (_, Nil) => true
      case _        => false
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Cons(_, t) => beginsWith(sup, sub) || hasSubsequence(t, sub)
      case Nil        => false
    }
}
