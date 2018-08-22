// Your name and ITU email: ____
package exams

import scala.language.higherKinds
import handIns.fpinscala.monoids.Monoid
import handIns.fpinscala.monads.Monad
import handIns.functionalState.State
import handIns.functionalState.State._
import handIns.stream.Exercise4
import handIns.stream.Exercise4._
import handIns.parallelism.Par
import handIns.fingerTree.FingerTree
import monocle.Lens

object exam2017 {
  object Q1 {
    // import exams.exam2017.Q1._
    // val test = List((3, "Jeppe"), (5, "Hotel"), (4, "Trivago"), (1, "Superman"),(2,"Batman"))

    // import exams.exam2017.Q1._
    // val test = List((3, "Jeppe"), (5, "Hotel"), (4, "Trivago"), (1, "Superman"),(2,"Batman"))

    def hasKey[K, V](l: List[(K, V)])(k: K): Boolean = l match {
      case Nil => false
      case h :: t => if (k == h._1) true else hasKey(t)(k)
    }

    def hasKey2[K, V](l: List[(K, V)])(k: K): Boolean =
      l.foldLeft(false)((b, kv) => if (kv._1 == k) true else b)


    private def f[K, V](ope: (V, V) => V)(result: List[(K, V)], el: (K, V)): List[(K, V)] =
      if (hasKey(result)(el._1))
        result.map({ case (k, v) => if (k == el._1) (k, ope(v, el._2)) else (k, v) })
      else el :: result

    def reduceByKey[K, V](l: List[(K, V)])(ope: (V, V) => V): List[(K, V)] =
      l.foldLeft(List[(K, V)]())(f(ope))


    def reduceByKey2[K, V](l: List[(K, V)])(ope: (V, V) => V): List[(K, V)] =
      l.foldLeft(List[(K, V)]())(
        (res, kv) =>
          if (hasKey(res)(kv._1))
            res.map({ case (k, v) => if (k == kv._1) (k, ope(v, kv._2)) else (k, v) })
          else kv :: res
      )

    def separate(l: List[(Int, List[String])]): List[(Int, String)] =
      l.flatMap(idws => idws._2.map(w => (idws._1, w)))


    def separateViaFor(l: List[(Int, List[String])]): List[(Int, String)] = for {
      idws <- l
      w <- idws._2
    } yield (idws._1, w)

  } // Q1


  object Q2 {

    import handIns.fpinscala.monads.Functor
    import scala.language.higherKinds

    trait TreeOfLists[+A]
    case object LeafOfLists extends TreeOfLists[Nothing]
    case class BranchOfLists[+A](
                                  data: List[A],
                                  left: TreeOfLists[A],
                                  right: TreeOfLists[A]
                                ) extends TreeOfLists[A]

    trait TreeOfCollections[C[+ _], +A]
    case class LeafOfCollections[C[+ _]]() extends TreeOfCollections[C, Nothing]
    case class BranchOfCollections[C[+ _], +A](data: C[A],
                                               left: TreeOfCollections[C, A],
                                               right: TreeOfCollections[C, A]
                                              ) extends TreeOfCollections[C, A]


    def map[A, B](t: TreeOfLists[A])(f: A => B): TreeOfLists[B] = t match {
      case LeafOfLists => LeafOfLists
      case BranchOfLists(data, left, right) =>
        BranchOfLists(data map f, map(left)(f), map(right)(f))
    }

    def map[A, B, C[+ _]](t: TreeOfCollections[C, A])(f: A => B)(implicit functorC: Functor[C]): TreeOfCollections[C, B] = t match {
      case LeafOfCollections() => LeafOfCollections()
      case BranchOfCollections(data, left, right) =>
        BranchOfCollections(functorC.map[A, B](data)(f), map(left)(f), map(right)(f))
    }

  } // Q2

  object Q3 {

    def p(n: Int): Int = {
      println(n.toString)
      n
    }

    def f(a: => Int, b: => Int): Int = if (a > 10) a else b

    // Answer the questions in comments here

    // A. p( f( p(42), p(7) ))
    //    42  in inner p
    //    7   in inner p
    //    42  in outer p

    // B. p( f( p(42), p(7) )))
    //    42  in f at a > 10
    //    42  in f at "a"
    //    42  in outer p

    // C. p( f( p(42), p(7) )))
    //    42  in f at a > 10
    //    42  in outer p

  } // Q3


  object Q4 {

    sealed trait Input
    case object Coin extends Input
    case object Brew extends Input

    case class MachineState(ready: Boolean, coffee: Int, coins: Int)

    def step(i: Input)(s: MachineState): MachineState = (i, s) match {
      case (_, MachineState(_, 0, _)) => s
      case (Brew, MachineState(true, _, _)) => s
      case (Brew, MachineState(false, coffee, coins)) =>
        MachineState(ready = true, coffee - 1, coins)
      case (Coin, MachineState(_, coffee, coins)) =>
        if (coffee > 0)
          MachineState(ready = false, coffee, coins + 1)
        else
          MachineState(ready = true, 0, coins + 1)
    }

    // Each input is passed as an argument to the step function, and the modify function then drives the change.
    // Note how modify takes an f: S => S and the step function has the signature
    // step(i: Input)(s: MachineState): MachineState. After the input has been passed to step, this is precisely
    // a function from S => S. Sequence then combines each of the preceding states into a single state, which
    // is retrieved using the get function.
    def simulateMachine(initial: MachineState)(inputs: List[Input]): (Int, Int) = {
      val stepper = for {
        _ <- sequence(inputs.map(i => modify[MachineState](step(i))))
        s <- get[MachineState]
      } yield (s.coffee, s.coins)
      stepper.run(initial)._1
    }

    def simulateMachine2(initial: MachineState)(inputs: List[Input]): (Int, Int) = {
      val ms: List[State[MachineState, Unit]] = inputs.map(i => modify(step(i)))
      val m: State[MachineState, Unit] = State sequence ms map (_ => Unit)
      val MachineState(ready, coffee, coins) = (m run initial)._2
      (coffee, coins)
    }


  } // Q4


  object Q5 {

    import handIns.stream.Exercise4.Stream._


    def flatten[A](s: => Stream[List[A]]): Stream[A] =
      s.flatMap(la => la.foldRight(Empty: Stream[A])((h, t) => cons(h, t)))

    // Notice how the signature requires us to change the type. Functions like map do not have this
    // capability, so we must specifically look for something that allows us to change the type.
    // The only signature that matches this requirement is flatMap:
    //
    //  def flatMap[B](f: A => Stream[B]): Stream[B] =
    //    foldRight(Empty: Stream[B])((h, t) => f(h) append t)
    //
    // flatMap is called on values in some context (here the context is a Stream) and unpacks it from the context.
    // This unpacked element is passed as the argument to f, which transforms the element in some way, and puts
    // it back into the context.
    //
    // The elements of the incoming Stream is unpacked from the context and passed as an argument to f,
    // which transforms the List into a Stream. This new stream contains all the elements of the list and
    // in the same order. The new stream is then appended to the tail, which eventually ends up being the
    // zero element 'Empty' (Stream is a Monad).
    //
    // I would like to make it clear what the tail in (h, t) in flatMap represents.
    // For the input Stream(h, t) where the forced stream is Stream(h, t1, t2, t3) and each element
    // h, t1, t2, t3 is a List, f: la => la.foldRight(Empty)((h, t) => cons(h, t))
    // with g: (h, t) => f(h) append t, we have:
    //
    // g(h,   t1.foldRight(Empty)(g))
    //   g(t1,  t2.foldRight(Empty)(g))
    //     g(t2,  t3.foldRight(Empty)(g))
    //       g(t3, Nil.foldRight(Empty)(g))
    //       Stream(t3*)
    //     Stream(t2*, t3*)
    //   Stream(t1*, t2*, t3*)
    // Stream(h*, t1*, t2*, t3*)


  } // Q5


  object Q6 {

    import handIns.parallelism.Par.Par
    import handIns.parallelism.Par._

    def parExists[A](as: List[A])(p: A => Boolean): Par[Boolean] =
      flatMap(parMap(as)(p))(lb => unit(lb.foldRight(false)(_ || _)))

    // parMap takes the input list and a function p, which is lifted using asyncF to the signature A => Par[B].
    // The predicate function is applied to every element in the list, which produces a list of lazyUnits.
    // Each element in the lazyUnit corresponds to the evaluation of the element in the predicate function, so
    // as.map(asyncF(f)) may produce a
    //
    //    List(lazyUnit(false), lazyUnit(true)): List[Par[Boolean]]
    //
    // This list is then passed as the argument to sequence, which uses map2 to extract the Future values in the
    // list with a foldRight, and then puts it back into a List[Boolean] using the _ :: _ construct.
    // This produces the list
    //
    //    es => UnitFuture(List(false, false, true)): Par[List[Boolean]]
    //
    // Then we use flatMap to unpack the List[Boolean] from the Par context and apply a function, that determines
    // if any of the elements are true. The result is put back into the context with unit, yielding a Par[Boolean].
    // Note that we can use map instead of flatMap and without the need of unit, as shown below.

    def parExists2[A](as: List[A])(p: A => Boolean): Par[Boolean] =
      map(parMap(as)(p))(lb => lb.foldRight(false)(_ || _))

    def parExists3[A](as: List[A])(p: A => Boolean): Par[Boolean] =
      map(parMap(as)(p))(lb => lb.exists(identity))


  } // Q6


  object Q7 {

    import handIns.fingerTree.FingerTree._

    // Implement this:
    def concatenate[A, B >: A](left: FingerTree[A])(right: FingerTree[B]): FingerTree[B] =
      reduceL[B, FingerTree[B]]{(leftTree, b) => leftTree addR b}(left, right)

  } // Q7


  object Q8 {

    def nullOption[T]: Lens[T, Option[T]] = Lens[T, Option[T]] {
      case null => None
      case c => Some(c)
    }(opT => _ => opT.getOrElse(null.asInstanceOf[T]))

    // Answer the questions below:

    // A. PutGet: lens.get(lens.set(r)(f)) == f
    // The PutGet law states that the set-function must capture all of the information
    // contained in the field, which has type Option[T]. Defining the setter function on a tuple
    // as si => _ => si._1 clearly does not capture the entire state of the input, as si._2 is not included.
    // To phrase it another way; PutGet fails in this case because some information contained in the field
    // does not get propagated to the record, namely si._2.
    //
    // The nullOption lens does satisfy the PutGet law. The usage of getOrElse ensures that the entire state
    // is captured in the setter function.


    // B. GetPut: lens.set(r)(lens get r) == r
    // The GetPut law states that the set-function is not allowed to have "side-effects". Defining the setter
    // as s => _ => (s, 0) for any lens, violates this law, as the side-effect value zero will be propagated
    // to the record, but the information is not contained in the field.
    //
    // There are no side-effects in the nullOption set-function and the law is obeyed.

    // C. PutPut: lens.set(lens.set(r)(f2))(f1) == lens.set(r)(f1)
    // The PutPut law states that, if you set a field twice with values f2 then f1, it is the same as setting
    // it once with value f1.
    //
    // The setter-function in nullOption is not sensitive to how many set-operations has been performed and
    // so the law is obeyed.

  } // Q8
}