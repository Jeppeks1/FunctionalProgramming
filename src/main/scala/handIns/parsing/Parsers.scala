package handIns.parsing

// I used Scala's standard library lists, and scalacheck Props in this set,
// instead of those developed by the book.

import java.util.regex._
import scala.util.matching.Regex
import language.higherKinds
import language.implicitConversions

/** A class for immutable linked lists representing ordered collections
  *  of elements of type `A`.
  *
  *  This class comes with two implementing case classes `scala.Nil`
  *  and `scala.::` that implement the abstract members `isEmpty`,
  *  `head` and `tail`.
  *
  *  This class is optimal for last-in-first-out (LIFO), stack-like access patterns. If you need another access
  *  pattern, for example, random access or FIFO, consider using a collection more suited to this than `List`.
  *
  *  $usesMutableState
  *
  *  ==Performance==
  *  '''Time:''' `List` has `O(1)` prepend and head/tail access. Most other operations are `O(n)` on the number of elements in the list.
  *  This includes the index-based lookup of elements, `length`, `append` and `reverse`.
  *
  *  '''Space:''' `List` implements '''structural sharing''' of the tail list. This means that many operations are either
  *  zero- or constant-memory cost.
  *  {{{
  *  val mainList = List(3, 2, 1)
  *  val with4 =    4 :: mainList  // re-uses mainList, costs one :: instance
  *  val with42 =   42 :: mainList // also re-uses mainList, cost one :: instance
  *  val shorter =  mainList.tail  // costs nothing as it uses the same 2::1::Nil instances as mainList
  *  }}}
  */
trait Parsers[ParseError, Parser[+ _]] {self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(a => succeed(f(a)))

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def succeed[A](a: A): Parser[A] =
    string("").map(_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
  }



  def manyA[A](p: Parser[A]): Parser[Int] =
  char('a').many.slice.map(_.length)

  def map2_[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
  map(p ** p2)(f.tupled)

  def many1[A](p: Parser[A]): Parser[List[A]] =
  map2(p, many(p))(_ :: _)

  val digitTimesA: Parser[Int] = for {
         digit <- "[0-9]+".r
           n = digit.toInt
           _ <- listOfN(n, char('a'))
  } yield n

  def product_[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
  flatMap(p)(a => map(p2)(b => (a, b)))

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = for {
         a <- p
           b <- p2
  } yield f(a, b)










  object Laws {

    // Storing the laws in the trait -- the will be instantiated when we have
    // concrete implementation.  Still without a concrete implementation they
    // can be type checked, when we compile.  This tells us that the
    // construction of the laws is type-correct (the first step for them
    // passing).

    import org.scalacheck._
    import org.scalacheck.Prop._

    val runChar: Prop = Prop.forAll { c: Char => run(char(c))(c.toString) == Right(c) }
    val runString: Prop = Prop.forAll { s: String => run(string(s))(s) == Right(s) }

    val listOfN1: Prop = Prop.protect(run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad"))
    val listOfN2: Prop = Prop.protect(run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab"))
    val listOfN3: Prop = Prop.protect(run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab"))
    val listOfN4: Prop = Prop.protect(run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad"))
    val listOfN5: Prop = Prop.protect(run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab"))
    val listOfN6: Prop = Prop.protect(run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab"))

    def succeed[A](a: A): Prop = Prop.forAll { s: String => run(self.succeed(a))(s) == Right(a) }

    // Not planning to run this (would need equality on parsers), but can write
    // for typechecking:

    def mapStructurePreserving[A](p: Parser[A]): Boolean =
      map(p)(a => a) == p
  }
}

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  // The first argument is the parsers implementation P (that we don't have).
  // We write this code with only having the interface
  def jsonParser[ParseErr, Parser[+ _]](P: Parsers[ParseErr, Parser]): Parser[JSON] = {

    import P._
    val spaces = char(' ').many.slice

    ??? /* Exercise 6 */
  }
}
