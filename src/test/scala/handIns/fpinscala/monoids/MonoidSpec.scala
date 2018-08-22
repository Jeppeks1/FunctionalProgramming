package handIns.fpinscala.monoids

// Example solution for scala exercises using scalacheck
// Scalacheck's user guide:
// https://github.com/rickynils/scalacheck/wiki/User-Guide

import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

object MonoidSpec extends Properties("Monoids..") {

  import Monoid._

  def associative[A: Arbitrary](m: Monoid[A]): Prop =
    forAll { (a1: A, a2: A, a3: A) =>
      m.op(m.op(a1, a2), a3) == m.op(a1, m.op(a2, a3))
    } :| "associativity"

  def unit[A: Arbitrary](m: Monoid[A]): Prop =
    forAll { a: A => m.op(a, m.zero) == a } :| "right unit" &&
      forAll { a: A => m.op(m.zero, a) == a } :| "left unit"

  def monoid[A: Arbitrary](m: Monoid[A]): Prop = associative(m) && unit(m)

  def homomorphism[A: Arbitrary, B: Arbitrary](ma: Monoid[A])(f: A => B)(mb: Monoid[B]): Prop =
  forAll { (a1: A, a2: A) =>
    mb.op(f(a1), f(a2)) == f(ma.op(a1, a2))
  } :| "homomorphism"

  def isomorphism[A: Arbitrary, B: Arbitrary](ma: Monoid[A])(f: A => B)(g: B => A)(mb: Monoid[B]): Prop =
  homomorphism(ma)(f)(mb) :| "Left homomorphism" && homomorphism(mb)(g)(ma) :| "Right homomorphism"


  property("stringMonoid is a monoid") = monoid(stringMonoid)
  property("intAddition is a monoid") = monoid(intAddition)
  property("intMultiplication is a monoid") = monoid(intMultiplication)
  property("booleanOr is a monoid") = monoid(booleanOr)
  property("booleanAnd is a monoid") = monoid(booleanAnd)
  property("option[Int] is a monoid") = monoid(optionMonoid[Int])
  property("productMonoid is a monoid") = monoid(productMonoid(optionMonoid[Int], listMonoid[String]))
  //property("endomorphic functions are monoids") = monoid(endoMonoid[Int => Int])

  property("string and int addition are homomorphic") = homomorphism(stringMonoid)(_.length)(intAddition)
  property("string and List[Char] are homomorphic") = homomorphism(stringMonoid)(_.toList)(listMonoid[Char])
  property("List[Char] and string are homomorphic") = homomorphism(listMonoid[Char])(_.mkString)(stringMonoid)

  property("List[Char] and string are isomorphic") = isomorphism(stringMonoid)(_.toList)(_.mkString)(listMonoid[Char])
  property("booleanOr and booleanAnd are isomorphic") = isomorphism(booleanOr)(x => !x)(x => !x)(booleanAnd)


}