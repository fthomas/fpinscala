package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def regex(r: Regex): Parser[String]

  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  def string(s: String): Parser[String]

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n > 0) map2(p, listOfN(n - 1, p))(_ :: _) else succeed(List.empty)

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _).or(succeed(List.empty[A]))

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def map[A,B](a: Parser[A])(f: A => B): Parser[B] =
    a.flatMap(va => succeed(f(va)))

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    p.flatMap(a => p2.map(b => f(a, b)))
    //product(p, p2).map(f.tupled)

  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    p.flatMap(a => p2.map(b => (a, b)))

  def succeed[A](a: A): Parser[A] =
    string("").map(_ => a)

  def nAndAs: Parser[String] =
    regex("""\d""".r)
      .map(Integer.valueOf)
      .flatMap(n => listOfN(n, string("a")))
      .map(_.mkString)

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many = self.many(p)
    def as[B](b: B): Parser[B] = self.map(p)(_ => b)
    def slice = self.slice(p)
    def ~>[B](p2: Parser[B]): Parser[B] = self.product(p, p2).map(_._2)
    def <~[B](p2: Parser[B]): Parser[A] = self.product(p, p2).map(_._1)
    def <~>[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
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

object Examples {
  trait JSON
  object JSON {
    case object JNull extends JSON
    case class JNumber(get: Double) extends JSON
    case class JString(get: String) extends JSON
    case class JBool(get: Boolean) extends JSON
    case class JArray(get: IndexedSeq[JSON]) extends JSON
    case class JObject(get: Map[String, JSON]) extends JSON
  }

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import JSON._
    import P._

    val spaces = char(' ').many.slice

    val jNull = string("null").as(JNull)

    val jBool = {
      val t = string("true").as(true)
      val f = string("false").as(false)
      (t or f).map(b => JBool(b))
    }

    val jString: Parser[JString] = ???

    val jNumber = regex("""-?(0|[1-9]\d*)(\.\d+|)([eE][+-]?\d+|)""".r)
      .map(s => JNumber(s.toDouble))

    def jValue: Parser[JSON] = jNull | jBool | jString | jNumber | jArray | jObject

    def jArray: Parser[JArray] = ???

    def jObject = {
      def colon = spaces ~> string(":") <~ spaces
      def jKeyValue = spaces ~> jString.map(_.get) <~ colon <~> jValue <~ spaces

      val open = spaces ~> string("{") <~ spaces
      val close = spaces ~> string("}") <~ spaces

      val empty = open ~> close as Map.empty[String, JSON]
      val nonEmpty = open ~> (jKeyValue <~> (string(",") ~> jKeyValue).many).map {
        case (x, xs) => (x :: xs).toMap
      } <~ close

      (empty | nonEmpty).map(m => JObject(m))
    }

    jValue
  }
}
