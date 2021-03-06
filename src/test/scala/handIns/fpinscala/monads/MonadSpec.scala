package handIns.fpinscala.monads

// Example solutions for Monad exercises, using scalacheck
// Scalacheck's user guide:
// https://github.com/rickynils/scalacheck/wiki/User-Guide

import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary
import scala.language.higherKinds
import Monad._


object MonadSpec extends Properties("Monad[F[_]] laws..") {

  // Note: The law is fine, but remember that scalacheck has presently a very
  // weak function generator (only generates constant functions)
/*
  def associative[A, F[_]](m: Monad[F])(implicit a: Arbitrary[F[A]]): Prop =
    forAll { (x: F[A], f: A => F[A], g: A => F[A]) =>
      m.flatMap[A, A](m.flatMap[A, A](x)(f))(g) ==
        m.flatMap(x)(a => m.flatMap(f(a))(g))
    }

  def identity[A, F[_]](m: Monad[F])(implicit arbFA: Arbitrary[F[A]],
                                     arbA: Arbitrary[A]): Prop =
    forAll { (x: F[A], f: A => F[A]) =>
      m.flatMap[A, A](x)(m.unit[A](_)) == x
    } :| "right unit" &&
      forAll { (y: A, f: A => F[A]) =>
        m.flatMap[A, A](m.unit[A](y))(f) == f(y)
      } :| "left unit"

  def monad[A, F[_]](m: Monad[F])(implicit arbFA: Arbitrary[F[A]],
                                  arbA: Arbitrary[A]): Prop =
    associative[A, F](m) && identity[A, F](m)
*/

  //property("of listMonad") = monad[Int, List](listMonad)
  //property("of optionMonad") = monad[Int, Option](optionMonad)
}
