package exams

import scala.language.higherKinds

import org.scalatest.FreeSpec
import org.scalatest.Matchers._
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import monocle.Lens

class exam2018Spec extends FreeSpec with Checkers {

  "QUESTION 2018.1 groupByKey" - {

    import exams.exam2018.Q1._

    "preserves keys" in check {
      forAll { l: List[(Int, Int)] =>
        l.map {_._1}.toSet == groupByKey(l).map {_._1}.toSet
      }
    }

    "preserves values" in check {
      forAll { l: List[(Int, Int)] =>
        l.map {_._2}.toSet == groupByKey(l).flatMap {_._2}.toSet
      }
    }

    "special scenarios" in {
      groupByKey(List((1, 1), (1, 1), (1, 2), (2, 3))) shouldBe List(1 -> List(1, 1, 2), 2 -> List(3))
      groupByKey(List((0, 0))) shouldBe List(0 -> List(0))
      groupByKey(Nil) shouldBe Nil
    }
  }

  "QUESTION 2018.2 either" - {

    import exams.exam2018.Q2._

    "pure failures survive" in check {
      forAll { l: List[Int] =>
        l.isEmpty || f(l map {Left(_)}) == Left(l)
      }
    }

    "pure successes survive" in check {
      forAll { l: List[Int] =>
        f(l map {Right(_)}) == Right(l)
      }
    }

    "when mixed failures survive" in check {
      forAll { l: List[Int] =>
        l.isEmpty || f((l map {Right(_)}) ++ (l map {Left(_)})) == Left(l)
      }
    }

    "scenarios" in {
      f(List()) shouldBe Right(Nil)
    }
  }

  "QUESTION 2018.8" - {

    import handIns.fingerTree._
    import exams.exam2018.Q8._

    "filtering on list and tree have same effect" in check {
      forAll { l: List[Int] =>
        val t = l.foldLeft[FingerTree[Int]](Empty()) { (t, a) => t addL a }
        filter(t) {_ % 2 == 0}.toList.toSet == l.filter {_ % 2 == 0}.toSet
      }
    }
  }

  import exams.exam2018.Q9._

  "QUESTION 2018.9" - {


    def PutGet[C: Arbitrary, A: Arbitrary](l: Lens[C, A]): Prop =
      forAll { (c: C, a: A) => l.get(l.set(a)(c)) == a }

    def GetPut[C: Arbitrary, A](l: Lens[C, A]): Prop =
      forAll { c: C => l.set(l.get(c))(c) == c }

    def PutPut[C: Arbitrary, A: Arbitrary](l: Lens[C, A]): Prop =
      forAll { (a: A, a1: A, c: C) => l.set(a1)(l.set(a)(c)) == l.set(a1)(c) }


    "eitherOption put-get" in check {
      PutGet(eitherOption[String, Int]("Fail"))
    }

    "eitherOption get-put" in check {
      GetPut(eitherOption[String, Int]("Fail"))
    }

    "eitherOption put-put" in check {
      PutPut(eitherOption[String, Int]("Fail"))
    }

  }


}


