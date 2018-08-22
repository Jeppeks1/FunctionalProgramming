package exams

// Name: _____________
// ITU email: ________

import scala.language.higherKinds
import handIns.fpinscala.monads.Monad
import handIns.parallelism.Par._
import handIns.parallelism.Par.Par
import handIns.fingerTree.FingerTree
import monocle.Lens

object exam2018 {
  object Q1 {

    def hasKey[K, V](l: List[(K, V)])(k: K): Boolean = l match {
      case Nil => false
      case h :: t => if (k == h._1) true else hasKey(t)(k)
    }

    def groupByKey[K, V](l: List[(K, V)]): List[(K, List[V])] =
      l.foldLeft(List[(K, List[V])]())((res, el) =>
        if (hasKey(res)(el._1))
          res.map({ case (k, lv) => if (k == el._1) (k, lv :+ el._2) else (k, lv) })
        else res :+ (el._1, List(el._2))
      )

    def groupByKey2[K, V](l: List[(K, V)]): List[(K, List[V])] =
      l.foldRight(List[(K, List[V])]())((el, res) =>
        if (hasKey(res)(el._1))
          res.map({ case (k, lv) => if (k == el._1) (k, el._2 :: lv) else (k, lv) })
        else (el._1, List(el._2)) :: res
      )


    // Note how the foldLeft operation access the elements from left to right. In order to maintain
    // the order of the elements, the first value that we access must stay in the front of the list.
    // Every subsequent element must therefore be appended rather than prepended to the list.
    //
    // The foldRight definition is reverse. The list is build from right to left, so we can just
    // prepend every element.

  }

  object Q2 {

    def f[A, B](results: List[Either[A, B]]): Either[List[A], List[B]] =
      results.collect { case Left(b) => b } match {
        case Nil => Right(results collect { case Right(a) => a })
        case failures => Left(failures)
      }

  }


  object Q3 {

    type T[B] = Either[String, B]
    implicit val eitherStringIsMonad: Monad[T] = new Monad[T] {
      override def unit[B](b: => B): Either[String, B] = Right(b)

      override def flatMap[B, C](ea: Either[String, B])(f: B => Either[String, C])
      : Either[String, C] = ea flatMap f
    }


    implicit def eitherIsMonad[A] = {
      type T[B] = Either[A, B]
      new Monad[T] {
        override def unit[B](b: => B): Either[A, B] = Right(b)

        override def flatMap[B, C](ea: Either[A, B])(f: B => Either[A, C])
        : Either[A, C] = ea flatMap f
      }
    }


  } // Q3


  object Q4 {

    //    * 1. The automaton generates Fibonacci numbers.  The state space contains
    //    *    two consecutive Fibonacci numbers, starting with (1,1) the first two
    //    *    numbers.  Each transition step produces the first of this numbers and
    //    *    advances the state to the next pair (dropping the first number, and
    //    *    adding a new second number produced by addition).  The prefix is:
    //    *
    //    *    1, 1, 2, 3, 5
    //    *
    //    * 2. The state space of the automaton are pairs of consecutive Fibonacci
    //    *    numbers (if starting with (1,1)). [pairs of integers]
    //    *
    //    * 3. The action range of the automaton are Fibonacci numbers. [positive
    //    *    integers, if starting with (1,1)]

  }


  object Q5 {

    def parForall[A](as: List[A])(p: A => Boolean): Par[Boolean] =
      flatMap(parMap(as)(p))(lb => unit(lb.forall(identity)))

    // This is equivalent to the above
    def parForall2[A](as: List[A])(p: A => Boolean): Par[Boolean] =
      map(parMap(as)(p))(lb => lb.forall(identity))


  }


  object Q6 {

    def apply[F[_], A, B](fab: F[A => B])(fa: F[A]): F[B] = ???

    def unit[F[_], A](a: => A): F[A] = ???

    val f: (Int, Int) => Int = _ + _

    def a: List[Int] = ???

    // Answer below in a comment:

    // apply(apply(unit(f.curried))(a))(a)
    // unit(f.curried): F[Int => Int => Int]
    // apply(fab: List[Int => Int => Int])(a: List[Int]): List[Int => Int]
    // apply(fab: List[Int => Int])(a: List[Int]): List[Int]

  } // Q6


  object Q7 {

    def map2[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = ???


    def map3[A, B, C, D](a: List[A], b: List[B], c: List[C])(f: (A, B, C) => D): List[D] = {
      map2(map2(a, b)({ case (aa, bb) => (aa, bb) }), c) { case ((aa, bb), cc) => f(aa, bb, cc) }
    }


    def map3monad[M[_] : Monad, A, B, C, D](a: M[A], b: M[B], c: M[C])(f: (A, B, C) => D): M[D] =
      implicitly[Monad[M]].map2(
        implicitly[Monad[M]].map2(a, b) { case (aa, bb) => (aa, bb) }, c) { case ((aa, bb), cc) => f(aa, bb, cc) }

    def map3monad2[M[_], A, B, C, D](a: M[A], b: M[B], c: M[C])(f: (A, B, C) => D)
                                    (implicit evidence: Monad[M]): M[D] =
      evidence.map2(
        evidence.map2(a, b) { case (aa, bb) => (aa, bb) }, c) { case ((aa, bb), cc) => f(aa, bb, cc) }

  } // Q7


  object Q8 {

    import handIns.fingerTree._

    def filter[A](t: FingerTree[A])(p: A => Boolean): FingerTree[A] =
      Digit.toTree(t.toList.filter(p))

  }


  object Q9 {

    def eitherOption[A, B](default: => A): Lens[Either[A, B], Option[B]] =
      Lens[Either[A, B], Option[B]](_.toOption)(optB => _ =>
        optB match {
          case None => Left(default)
          case Some(b) => Right(b)
        })


    // Answer the questions below:

    // A. PutGet: lens.get(lens.set(r)(f)) == f
    // The PutGet law states that the set-function must capture all of the information
    // contained in the field, which has type Option[T]. Defining the setter function on a tuple
    // as si => _ => si._1 clearly does not capture the entire state of the input, as si._2 is not included.
    // To phrase it another way; PutGet fails in this case because some information contained in the field
    // does not get propagated to the record, namely si._2.
    //
    // The eitherOption lens does satisfy the PutGet law. The usage of the match function ensures that
    // both the None and Some state is captured in the setter function, as it is an exhaustive match.
    // The _.toOption getter is responsible for translating the Either to an Option, so that the f on
    // both sides of the law, has the same Some/None value.


    // B. GetPut: lens.set(r)(lens get r) == r
    // The GetPut law states that the set-function is not allowed to have "side-effects". Defining the setter
    // as s => _ => (s, 0) for any lens, violates this law, as the side-effect value zero will be propagated
    // to the record, but the information is not contained in the field.
    //
    // The lens eitherOption does have a side-effect and the lens does not obey the GetPut law.
    // We can see this, by passing in the record Left(?) with any value that is not the default value.
    // The 'lens get r' will produce a None when passed a Left(?). The next call 'lens.set(r)(None)' will
    // then see the None and use the default value, resulting in the record Left(default). The law will therefore
    // compare Left(default) == Left(?) and the law fails for any ? different from the default value.
    //
    // Or put in another way: the default value will be propagated to the record, but the default value is not
    // always contained in the field.


    // C. PutPut: lens.set(lens.set(r)(f2))(f1) == lens.set(r)(f1)
    // The PutPut law states that, if you set a field twice with values f2 then f1, it is the same as setting
    // it once with value f1.
    //
    // The setter-function in eitherOption is not sensitive to how many set-operations has been performed and
    // so the law is obeyed. This can also be seen by noting, that the second parameter in the set function
    // is ignored, so it always produces the same result for a given input field.



  } // Q9

}

