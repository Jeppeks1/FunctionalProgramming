/**
  * Advanced Programming.  Fake exam 2016.  This set of exam questions have been
  * prepared as a demonstration before the first written exam has been used in
  * ADPRO.
  */

package exams

object exam2016fake {
  object Q1 {

    import scala.collection.mutable.ListBuffer

    /**
      * Task 1.
      *
      * Translate the following scala function to a referentially
      * transparent version. You don't need to know what is a differential to
      * solve this task.
      */

    def listDifferentialImp(inList: ListBuffer[Int]): ListBuffer[Int] = {

      var result = ListBuffer[Int]()
      if (inList.size > 1) {
        var prev = inList.head
        for (curr <- inList.tail) {
          result += curr - prev // '+=' adds the right hand side as the last element in the list buffer
          println("curr: " + curr + " and prev: " + prev)
          prev = curr
        }
      }
      result
    }

    def listDifferentialFun(inList: List[Int]): List[Int] = {
      def loop(list: List[Int], res: List[Int]): List[Int] = {
        if (list.size > 1)
          loop(list.drop(1), res :+ (list.drop(1).head - list.head))
        else
          res
      }

      loop(inList, List())
    }


  }


  object Q2 {

    /**
      * Task 2.
      *
      * Implement  function onList  that  converts any  function of  type
      * String=>String to a function  of type List[Char]=>List[Char] that
      * satisies the following property:
      *
      * For any String s, and any function f:String => String:
      *
      * f(s) == ( (onList (f)) (s.toList) ).mkString
      *
      * where mkString is  a function that converts (implodes)  a list of
      * characters back to a String.
      */

    def onList(f: String => String): List[Char] => List[Char] =
      cs => f(cs.mkString).toList

  }


  object Q3 {

    import handIns.fpinscala.monoids.Monoid
    import scala.language.higherKinds

    /**
      * Task 3.
      *
      * Implement a function foldBack that  folds an operator of a monoid
      * M, by traversing  through the list twice. If the  operator is "+"
      * and  the List  is  : List(x1,x2,x3),  then  your function  should
      * compute:
      *
      * (((((((z + x1) + x2) +x3) + x3) + x2) + x1) + z)
      *
      * where z = M.zero and + is M.op .
      */

    def foldBack[A](l: List[A])(implicit M: Monoid[A]): A =
      (l ++ l.reverse).foldLeft(M.zero)(M.op)

  }


  object Q4 {

    /**
      * Task 4.
      *
      * (One does not need to know probability theory to solve this task).
      *
      * Let  trait Event  be a  trait representing  random events  (as in
      * probability theory)  and P  be a probability  function, assigning
      * a  value  in  the  interval  [0;1] to  each  event  (an  instance
      * of  Event). Assume  the  declarations  below. The body  of  P  is
      * irrelevant.
      */

    trait Event

    trait Probability

    def P(e: Event): Probability = ??? // assume that this is defined

    /**
      * The   function   conditionalP(E1,E2)    assigns   a   conditional
      * probability value  to a pair  of random  events E1 and  E2.  This
      * function  is sometimes  undefined.  Write  the type  signature of
      * conditionalP below.
      *
      * Note that  we are not asking  for a definition of  this function,
      * just for a type declaration.
      */

    def conditionalP(E1: Event, E2: Event): Option[Probability] = ???

  }


  object Q5 {

    /**
      * Task 5.
      *
      * Consider a type of lazy binary trees:
      */

    trait Tree[+A]

    case class Branch[+A](l: () => Tree[A], r: () => Tree[A]) extends Tree[A]

    case object Leaf extends Tree[Nothing]

    /**
      * Implement a  convenience constructor  'branch' that is  both lazy
      * but does not require using explicit delays like Branch.
      */

    def branch[A](l: => Tree[A], r: => Tree[A]): Tree[A] = Branch(() => l, () => r)

  }


  object Q6 {

    import monocle.Optional

    /**
      * Task 6.
      *
      * Formalize a lense leftFT, that allows accessing and replacing the
      * leftmost element of a deque stored in a finger tree.
      *
      * Recall the basic types from our implementation:
      */

    trait FingerTree[+A] {
      def addL[B >: A](b: B): FingerTree[B] = ??? // assume that this is implemented
    }

    case class Empty() extends FingerTree[Nothing]

    sealed trait ViewL[+A]

    case class NilTree() extends ViewL[Nothing]

    case class ConsL[A](hd: A, tl: FingerTree[A]) extends ViewL[A]

    def viewL[A](t: FingerTree[A]): ViewL[A] = ??? // assume that this is defined

    /* Use the addL and viewL to create a lens that extracts and allows
   * to modify the left most element of a finger tree. Either use the Monocle
   * API or (if you are writing in free text) use the notation from the paper of
   * Foster et al.
   *
   * Include the type of the lens (partial/total), and the put and get function.
   */

    // Outcommented due to compilation errors, as the above definitions are not implemented.
    // def leftFT[A]: Optional[FingerTree[A], A] = Optional[FingerTree[A], A](t => viewL(t) match {
    //   case NilTree() => None
    //   case ConsL(hd, _) => Some(hd)
    // })(a => {
    //   case NilTree() => Empty().addL[A](a)
    //   case ConsL(_, tl) => tl.addL[A](a)
    // })


  }


  object Question7 {

    import handIns.generator.Gen

    /**
      * Task 9.
      *
      * Implement  a generator multiplesOf10 that generates
      * integer numbers that are divisible by 10.
      *
      * Assume an implementation of Gen[A] as in the text book.
      * Also assume existance of arbitraryInt (implemented)
      *
      * Provide an explicit type for multiplesOf10
      */

    val arbitraryInt: Gen[Int] = ??? // assume that this exists.

    val multiplesOf10: Gen[Int] = arbitraryInt.map(n => (n / 10) * 10)


    /**
      * Task 10.
      *
      * Implement a generator multipleOf10UpTo(m) that generaters integer
      * numbers divisible by 10, but smaller than m.
      *
      * Provide an explicit type for multiplesOf10UpTo
      */

    def multiplesOf10UpTo(m: Int): Gen[Int] = arbitraryInt.map {
      n => ((n / 10) % (m / 10)) * 10
    }

  }


  object Q8 {

    import handIns.generator.Gen
    import handIns.functionalState.RNG

    val arbitraryInt: Gen[Int] = ??? // assume that this exists.
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = ??? // assume that this exists.

    /**
      * Task 11.
      *
      * Below you will find two expressions that (apparently) generate randomly
      * sized integer lists.
      *
      * Write explicit types for v2 and v1.
      *
      * Explain in English (or Danish) what are the types of values v1 and v2 and
      * explain the difference between the computations that produce them.  The
      * explanation should not be long (4-5 lines will suffice).
      **/

    val v1: Gen[List[Int]] = arbitraryInt.flatMap(n => listOfN(n, arbitraryInt))
    val v2 = arbitraryInt.flatMap(n => listOfN(n, arbitraryInt)).
      sample.run(RNG.Simple(42))

    //v2: (List[Int], RNG)

    /* The expression computing v1 constructs a generator, it does not actually run
   * any generation.  The expression computing v2 uses the generator constructed
   * in v1 by actually executing it (given a random seed). So the actual
   * generation happens in the second example.
   */

  }
}