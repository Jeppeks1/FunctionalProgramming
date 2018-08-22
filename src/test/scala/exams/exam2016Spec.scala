package exams

import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._


class exam2016Spec extends FlatSpec with Checkers {

  import exams.exam2016._

  behavior of "Q1: checksumImp"

  it should "return zero for empty String" in {
    Q1 checksumImp "" shouldBe 0
  }
  it should "return 65 for \"A\"" in {
    Q1 checksumImp "A" shouldBe 65
  }
  it should "return 198 for \"ABC\"" in {
    Q1 checksumImp "ABC" shouldBe (65 + 66 + 67)
  }


  behavior of "Q1: checksumImp and checksumFun"

  it should "behave the same" in check {
    forAll { s: String =>
      (Q1 checksumImp s) == (Q1 checksumFun s)
    }
  }

  behavior of "Q2: onList"
  it should "toUpper a list" in {
    Q2.onList[Char](_.toUpper) ("abc".toList).mkString shouldBe "ABC"
  }

  import handIns.fpinscala.monads.Functor.OptionFunctor
  import handIns.fpinscala.monads.Functor.ListFunctor

  behavior of "Q2: onCollection"
  it should "toUpper an Option" in {
    (Q2.onCollection[Option, Char]
      (_.toUpper)(OptionFunctor)) (Some('a')) shouldBe
      Some('A')
  }

  it should "toUpper a List" in {
    (Q2.onCollection[List, Char]
      (_.toUpper)(ListFunctor)) (List('a', 'b')) shouldBe
      List('A', 'B')
  }


  import handIns.fpinscala.monoids.Monoid.intAddition
  import handIns.fpinscala.monoids.Monoid.stringMonoid

  behavior of "Q3"
  it should "equal to summing twice for int monoid" in check {
    forAll { l: List[Int] =>
      Q3.foldBack(l)(intAddition) == l.foldLeft(intAddition.zero)(intAddition.op) * 2
    }
  }

  it should "should work for a non-commutative monoid too" in check {
    forAll { l: List[String] =>
      Q3.foldBack(l)(stringMonoid) ==
        (l ++ l.reverse).foldRight(stringMonoid.zero)(stringMonoid.op)
    }
  }


}


