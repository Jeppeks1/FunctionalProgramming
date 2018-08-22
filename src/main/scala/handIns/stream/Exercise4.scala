package handIns.stream

object Exercise4 extends App {
  sealed trait Stream[+A] {

    import Stream._

    def headOption(): Option[A] =
      this match {
        case Empty => None
        case Cons(h, _) => Some(h())
      }

    def tail: Stream[A] = this match {
      case Empty => Empty
      case Cons(_, t) => t()
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Empty => z
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      // Note 1. f can return without forcing the tail
      // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
      // if f requires to go deeply into the stream. So folds sometimes may be
      // less useful than in the strict case
    }

    // Note 1. eager; cannot be used to work with infinite streams. So foldRight
    // is more useful with streams (somewhat opposite to strict lists)
    def foldLeft[B](z: => B)(f: (A, => B) => B): B = this match {
      case Empty => z
      case Cons(h, t) => t().foldLeft(f(h(), z))(f)
      // Note 2. even if f does not force z, foldLeft will continue to recurse
    }

    def exists(p: A => Boolean): Boolean = this match {
      case Empty => false
      case Cons(h, t) => p(h()) || t().exists(p)
      // Note 1. lazy; tail is never forced if satisfying element found this is
      // because || is non-strict
      // Note 2. this is also tail recursive (because of the special semantics
      // of ||)
    }


    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => Empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case Cons(h, t) => Cons(h, t)
      case _ => Empty
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => Empty
    }


    def forAll(p: A => Boolean): Boolean = this match {
      case Cons(h, t) if p(h()) => t().forAll(p)
      case Cons(h, _) if !p(h()) => false
      case _ => true
    }


    def takeWhileFoldRight(p: A => Boolean): Stream[A] =
      foldRight(Empty: Stream[A])((h, t) => if (p(h)) cons(h, t) else Empty)

    def headOptionFoldRight: Option[A] =
      foldRight(None: Option[A])((h, _) => Some(h))

    def map[B](f: A => B): Stream[B] =
      foldRight(Empty: Stream[B])((h, t) => cons(f(h), t))

    def filter(p: A => Boolean): Stream[A] =
      foldRight(Empty: Stream[A])((h, t) => if (p(h)) cons(h, t) else t)

    def append[B >: A](that: => Stream[B]): Stream[B] =
      foldRight(that)((h, t) => cons(h, t))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Empty: Stream[B])((h, t) => f(h) append t)

    def find(p: A => Boolean): Option[A] = this.filter(p).headOption()


  }


  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


  object Stream {

    def empty[A]: Stream[A] = Empty

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq


    def to(n: Int): Stream[Int] =
      if (n < 0) empty else cons(n, to(n - 1))

    def from(n: Int): Stream[Int] =
      if (n < 0) empty else cons(n, from(n + 1))

    val naturals: Stream[Int] = from(0)

    def fibs(n: Int): Stream[Int] = {
      def go(f0: Int, f1: Int): Stream[Int] = {
        cons(f0, go(f1, f0 + f1))
      }

      go(0, 1)
    }


    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => Empty
    }


    def fibsUnfold: Stream[Int] =
      unfold((0, 1)) { case (f0, f1) =>
        Some((f0, (f1, f0 + f1)))
      }


  }
}