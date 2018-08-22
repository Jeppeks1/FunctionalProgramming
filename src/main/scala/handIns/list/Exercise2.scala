package handIns.list

/** A class for immutable linked lists representing ordered collections
  *  of elements of type `A`.
  *
  *  This class comes with a case object `Nil` and a case class `Cons` that handles the construction of lists.
  *
  *  ==Performance==
  *  '''Time:''' `List` has `O(1)` prepend and head/tail access. Most other operations are `O(n)` on the number of elements in the list.
  *  This includes the index-based lookup of elements, `length`, `append` and `reverse`.
  *
  *  '''Space:''' `List` implements '''structural sharing''' of the tail list. This means that many operations are either
  *  zero- or constant-memory cost.
  *  {{{
  *  val mainList = List(3, 2, 1)
  *  val with4 =    4 :: mainList  // re-uses mainList, costs one :: instance
  *  val with42 =   42 :: mainList // also re-uses mainList, cost one :: instance
  *  val shorter =  mainList.tail  // costs nothing as it uses the same 2::1::Nil instances as mainList
  *  }}}
  *
  *  @example {{{
  *  // Make a list via the companion object factory
  *  val days = List("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  *
  *  // Make a list element-by-element
  *  val when = "AM" :: "PM" :: List()
  *
  *  // Pattern match
  *  days match {
  *    case firstDay :: otherDays =>
  *      println("The first day of the week is: " + firstDay)
  *    case List() =>
  *      println("There don't seem to be any week days.")
  *  }
  *  }}}
  */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  /** Convenience function to construct a list consisting of each input argument.
    *
    * @param as elements to wrap in a list.
    * @return a list which contains the elements ´as´.
    */
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))



  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  // Exercise 4

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  // Exercise 5

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // Exercise 6

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, value) => value + 1)

  // Exercise 7

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // Exercise 8

  def product(as: List[Double]): Double =
    foldLeft(as, 1.0)(_ * _)

  def length1 (as :List[Int]) : Int =
    foldLeft(as, 0)((_, value) => value + 1)

  // Exercise 9

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((h,t) => Cons(t,h))

  // Exercise 10

  def foldRight1[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(as), z)((b,a) => f(a,b))

  def foldLeft1[A,B](as: List[A], z: B)(f: (B,A) => B): B =
    foldRight(as, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  // Exercise 11

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def concat[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil:List[A])(append)

  // Exercise 12

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h, t) if  f(h) => filter(t)(f)
    case Cons(h, t) if !f(h) => Cons(h, filter(t)(f))
    case _ => as
  }

  // Exercise 13

  def map[A,B](as: List[A])(f: A => B): List[B]  = as match {
    case Nil => Nil
    case Cons(h, Nil) => Cons(f(h), Nil)
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  // Exercise 14

  def filter1[A] (as: List[A]) (f: A => Boolean) :List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  // Exercise 15
  def add(as: List[Int]) (bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, at), Cons(b, bt)) => Cons(a + b, add(at)(bt))
  }

  // Exercise 16
  def zipWith[A,B,C](f: (A,B) => C)(a: List[A], b: List[B]): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(f)(t1,t2))
  }

  // Exercise 17
  // Note: We ended up getting stuck after defining the skeleton of hasSubsequence.
  // The solutions manual was used to complete this exercise.
  def startsWith[A](as: List[A], pre: List[A]): Boolean = (as,pre) match {
    case (_, Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case Cons(_,_) if startsWith(sup, sub) => true
    case Cons(_,t) => hasSubsequence(t, sub)
  }

  // Exercise 18
  // Not implemented

  // a test: pascal (4) = Cons(1,Cons(3,Cons(3,Cons(1,Nil))))



}