package handIns.fpinscala.monoids

import scala.language.higherKinds
import Monoid._

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  def stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    val zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    val zero: Nil.type = Nil
  }

   def intAddition: Monoid[Int] = new Monoid[Int]{
     def op(a1: Int, a2: Int): Int = a1 + a2
     val zero = 0
   }

  def intMultiplication: Monoid[Int] = new Monoid[Int]{
    def op(a1: Int, a2: Int): Int = a1 * a2
    val zero = 1
  }

  def booleanOr: Monoid[Boolean] = new Monoid[Boolean]{
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    val zero = false
  }

  def booleanAnd: Monoid[Boolean] = new Monoid[Boolean]{
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    val zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]]{
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    val zero: None.type = None
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(a1: A, a2: A): A = m.op(a2, a1)
    val zero: A = m.zero
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A]{
    def op(f: A => A, g: A => A): A => A = f compose g
    val zero: A => A = (a: A) => a
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  // Exercise 7
  //
  // Implement a productMonoid that builds a monoid out of two monoids. Test it
  // with scala check for instance by composing an Option[Int] monoid with a
  // List[String] monoid and running through our monoid laws.

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A,B)] = new Monoid[(A, B)] {
    def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
    def zero: (A, B) = (A.zero, B.zero)
  }


}


trait Foldable[F[_]] {

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}


object Foldable extends Foldable[List] {

  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
}
