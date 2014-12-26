package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

  def map[A,B](a: Parser[A])(f: A => B): Parser[B]

  def many[A](p: Parser[A]): Parser[List[A]]

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)]

  def string(s: String): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] =
    product(p, p2).map(f.tupled)

  case class ParserOps[A](p: Parser[A]) {
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def productLeftIdentity[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, product(string(""), p).map(_._2))(in)

    def productRightIdentity[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, product(p, string("")).map(_._1))(in)

    def productAssociativity[A, B, C](p1: Parser[A], p2: Parser[B], p3: Parser[C])(in: Gen[String]): Prop = {
      val lhs = product(product(p1, p2), p3).map { case ((a, b), c) => (a, b, c) }
      val rhs = product(p1, product(p2, p3)).map { case (a, (b, c)) => (a, b, c) }
      equal(lhs, rhs)(in)
    }

    def productMap[A, B, C, D](p1: Parser[A], p2: Parser[B], f: A => C, g: B => D)(in: Gen[String]): Prop = {
      val c1 = product(p1, p2).map(x => (f(x._1), g(x._2)))
      val c2 = product(p1.map(f), p2.map(g))
      equal(c1, c2)(in)
    }
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}