package exams


import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary




object ExamSpec extends Properties("Exam questions.."){

  import exams.exam2016.Q1._

  def checksum1: Prop =
    forAll { a: String =>
      checksumImp(a) == checksumFun(a)
    }

  def checksum2: Prop =
  forAll { a: String =>
    checksumFun(a) == checksumLoop(a)
  }
  property("Imperative and functional are identical") = checksum1
  property("Functional and loop are identical") = checksum2


}
