package exams

import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary


class exam2016fakeSpec extends FlatSpec with Checkers {

  import scala.collection.mutable.ListBuffer
  import exams.exam2016fake._

  behavior of "Q1"
  it should "work on empty list" in {
    Q1.listDifferentialImp(ListBuffer[Int]()) should have size 0
  }

  it should "work on singleton list" in {
    Q1.listDifferentialImp(ListBuffer(42)) should have size 0
  }

  it should "work on a longer list" in {
    Q1.listDifferentialImp(ListBuffer(1, 1, 2, 4, 7, 11, 42)) shouldBe
      ListBuffer(0, 1, 2, 3, 4, 31)
  }

  behavior of "Imp and Fun"
  it should "be same" in check {
    forAll { in: ListBuffer[Int] =>
      Q1.listDifferentialImp(in).toList ==
        Q1.listDifferentialFun(in.toList)
    }
  }

  behavior of "Q2"
  it should "satisfy the law specified int the task" in check {
    forAll { (f: String => String) =>
      forAll { s: String =>
        f(s) == Q2.onList(f)(s.toList).mkString
      }
    }
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