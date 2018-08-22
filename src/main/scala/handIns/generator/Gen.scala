package handIns.generator

import handIns.functionalState.State
import handIns.functionalState.RNG

// A generator will use a random number generator RNG in its state, to create
// random instances (but perhaps also some other stuff)
case class Gen[A](sample: State[RNG, A]) {

  // Let's convert generator to streams of generators
  def toStream(seed: Long): Stream[A] =
    Gen.state2stream(this.sample)(RNG.Simple(seed))

  def toStream(rng: RNG): Stream[A] =
    Gen.state2stream(this.sample)(rng)

  def listOfN(n: Int): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(this.sample)))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(a => listOfN(a))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def map[B](f: A => B): Gen[B] =
    this.flatMap(a => Gen.unit(f(a)))


  // Exercise 6 (Ex. 8.7; I implemented it as a method, the book asks for a
  // function, the difference is minor; you may want to have both for
  // convenience)
  //
  // Hint: we already have a generator that emulates tossing a coin. Which one
  // is it? Use flatMap with it.

  def union(that: Gen[A]): Gen[A] =
    Gen.boolean.flatMap(a => if (a) this else that)

  // Exercise 7 continues in the bottom of the file (in the companion object)
}

object Gen {

  // A convenience function to convert states (automata) to streams (traces)
  // It would be better to have it in State, but I am not controlling
  // State.scala.

  private def state2stream[A](s: State[RNG, A])(seed: RNG): Stream[A] =
    s.run(seed) match {
      case (n, s1) => n #:: state2stream(s)(s1)
    }

  def anyInteger: Gen[Int] = Gen(State(_.nextInt))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def double: Gen[Double] = Gen(State(RNG.double))
}

// This is the Prop type implemented in [Chiusano, Bjarnasson 2015]

object Prop {

  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  // the type of results returned by property testing

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      as.toStream(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

import Prop._

case class Prop(run: (TestCases, RNG) => Result) {

  // (Exercise 7)

  def &&(that: Prop): Prop = Prop {
    (n, rng) =>
      run(n, rng) match {
        case Passed => that.run(n, rng)
        case x => x
      }
  }

  def ||(that: Prop): Prop = Prop {
    (n, rng) =>
      run(n, rng) match {
        case Falsified(msg, _) => that.tag(msg).run(n, rng)
        case x => x
      }
  }

  def tag(msg: String): Prop = Prop {
    (n, rng) =>
      run(n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
  }

}

