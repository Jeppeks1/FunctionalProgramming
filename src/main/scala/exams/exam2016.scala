package exams

import scala.annotation.tailrec

object exam2016 {

  object Q1 {
    def checksumImp(in: String): Int = {
      var result = 0

      for (c <- in.toList)
        result = (result + c.toInt) % 0xffff

      result
    }

    def checksumLoop(in: String): Int = {
      def loop(n: Int, res: Int): Int = {
        if (n < 1) res
        else loop(n - 1, (res + in.charAt(n - 1)) % 0xffff)
      }

      loop(in.length, 0)
    }

    def checksumFun(in: String): Int =
      in.toList.foldLeft(0)((r, c) => (r + c.toInt) % 0xffff)


    // The method checksumFun is not tail-recursive, as recursion is not used in
    // the foldLeft method supplied by the standard library. The main benefit of
    // tail-recursion is to avoid stack overflow errors, which foldLeft accomplishes
    // as well.
    // The method checksumLoop is tail-recursive, as the recursive call
    // loop(n - 1, (res + in.charAt(n - 1)) % 0xffff) is the only operation left
    // and no further work needs to be done.

  }


  object Q2 {

    import handIns.fpinscala.monads.Functor
    import scala.language.higherKinds

    def onList[A](f: A => A): List[A] => List[A] =
      a => a.map(f)



    def onCollection[C[_], A](f: A => A)
                             (implicit functorC: Functor[C]): C[A] => C[A] =
      ca => functorC.map(ca)(f)

  }

  object Q3 {

    import handIns.fpinscala.monoids.Monoid
    import scala.language.higherKinds

    def foldBack[A](l: List[A])(implicit M: Monoid[A]): A =
      (l ++ l.reverse).foldRight(M.zero)(M.op)

    def foldBack2[A](l: List[A])(implicit M: Monoid[A]): A =
      l.foldRight(l.reverse.foldRight(M.zero)(M.op))(M.op)

    // Note that this only produces (x1 + (x2 + (x3 + (x3 + (x2 + (x1 + z))))))
    // and the very first z + ... is not included. As the monoids must be
    // associative, we know that z + a = a and so the first z is not necessary.
    // To completely follow the specifications of the exercise, one could write
    // M.op(M.Zero, l.foldRight(l.reverse.foldRight(M.zero)(M.op))(M.op)) instead.
  }

  object Q4 {

    type Computation[A] = A => Either[String, A]

    def run[A](init: A)(progs: List[Computation[A]]): (A, List[String]) = {
      progs.reverse.foldRight((init, List[String]()))((ca, res) =>
        ca(res._1) match {
          case Left(str) => (res._1, str :: res._2)
          case Right(v) => (v, res._2)
        }
      )
    }

    def compute[A](a: Int): Either[String, Int] = {
      if (a <= 10) Right(a + 1)
      else if (a == 11) Left("Error: a == 11")
      else if (a == 12) Right(a + 1)
      else if (a == 13) Left("Error: a == 13")
      else if (a == 14) Right(a + 1)
      else Left("Error: a >= 15")
    }

    def addOne[A](a: Int): Either[String, Int] = {
      Right(a + 1)
    }

//  Use case:
//
//  import exams.exam2016.Q4._
//  val progs1: List[Int => Either[String,Int]] = List(compute, compute, compute, compute)
//  val progs2: List[Int => Either[String,Int]] = List(addOne, compute, compute, addOne)
//  val progs3: List[Int => Either[String,Int]] = progs1 ::: progs2
//  val res = run(8)(progs3)



  }


  object Q5 {

    sealed trait Tree[A]
    case class Branch[A](l: () => Tree[A], a: A, r: () => Tree[A]) extends Tree[A]
    case class Leaf[A](a: A) extends Tree[A]

    def multiply(t: Tree[Int]): Int = t match {
      case Leaf(a) => a
      case Branch(l, a, r) =>
        if (a == 0) 0
        else {
          val ml = multiply(l())
          if (ml != 0) ml * a * multiply(r())
          else 0
        }
    }

//    Use case:
//    import exams.exam2016.Q5.Tree
//    import exams.exam2016.Q5._
//
//    val tree = Branch(() => Branch(() => Leaf(5), 4, () => Leaf(2)), 8 , () => Branch(() => Leaf(1), 6, () => Leaf(3)))
//                8
//        4               6
//    5       2       1       3


    // Task 8. (answer below in a comment)
    // For correctness, I would write a .toList function on the Tree structure and
    // use a foldLeft to compute the result and then compare the results. For laziness,
    // I would construct a method that generates an infinite branch on one side,
    // and a small finite branch with a zero on the other side. If the test terminates, the
    // function is lazy. If the test does not terminate, flip the infinite and
    // finite branch around and retry.

    def infiniteTree: Tree[Int] = {
      Branch(() => infiniteTree , 1 , () => infiniteTree)
    }

    def infiniteLeftTree: Tree[Int] = Branch(() => Branch(() => Leaf(0), 4, () => infiniteTree), 8 , () => Branch(() => Leaf(1), 6, () => Leaf(3)))
    //                8
    //        4               6
    //    0      inf      1       3

    def infiniteRightTree: Tree[Int] = Branch(() => Branch(() => Leaf(5), 4, () => Leaf(2)), 8 , () => Branch(() => infiniteTree, 6, () => Leaf(0)))
    //                8
    //        4               6
    //    5      2       inf      0




  }

  object Q6 {

    sealed trait Nat[+A]
    case object Zero extends Nat[Unit]
    case class Succ[A](pred: A) extends Nat[A]

    val zero: Nat[Unit] = Zero // Task 9.
    val one: Succ[Nat[Unit]] = Succ(zero) // Task 9.
    val two: Succ[Succ[Nat[Unit]]] = Succ(one) // Task 9.


    def plus2[A](x: Nat[_]): Nat[_] = {
      Succ(Succ(x))
    }

  }
}