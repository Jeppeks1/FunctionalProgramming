package handIns.option

// Chapter 3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 2 (3.25)
  def size[A](t: Tree[A]): Int = t match {
    case Branch(left, right) => 1 + size(left) + size(right)
    case Leaf(_) => 1
  }

  // Exercise 3 (3.26)
  def maximum(t: Tree[Int]): Int = t match {
    case Branch(left, right) => maximum(left) max maximum(right)
    case Leaf(value) => value
  }

  // Exercise 4 (3.28)
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    case Leaf(value) => Leaf(f(value))
  }

  // Exercise 5 (3.29)
  def fold[A, B](t: Tree[A])(f: (B, B) => B)(g: A => B): B = t match {
    case Branch(left, right) => f(fold(left)(f)(g), fold(right)(f)(g))
    case Leaf(value) => g(value)
  }

  // Fixed type inference, note that we first tell Scala how to produce
  // B's before we use them.
  def fold1[A, B](t: Tree[A])(g: A => B)(f: (B, B) => B): B = t match {
    case Branch(left, right) => f(fold1(left)(g)(f), fold1(right)(g)(f))
    case Leaf(value) => g(value)
  }

  def size1[A](t: Tree[A]): Int =
    fold(t)((l: Int, r: Int) => 1 + l + r)(_ => 1)

  def maximum1[A](t: Tree[Int]): Int =
    fold(t)((l: Int, r: Int) => l max r)(v => v)

  // Type inference error
  //  def map1[A, B](t: Tree[A])(f: A => B): Tree[B] =
  //    fold(t)(Branch(_, _))(v => Leaf(f(v)): Tree[B])

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold1(t)(v => Leaf(f(v)): Tree[B])(Branch(_, _))

}

sealed trait Option[+A] {

  // Exercise 6 (4.1)
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(v) => f(v)
  }

  def flatMap2[B](f: A => Option[B]): Option[B] =
    this map f getOrElse None

  def filter(f: A => Boolean): Option[A] =
    flatMap(v => if (f(v)) Some(v) else None)

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object ExercisesOption {
  import handIns.list._

  // Remember that mean is implemented in Chapter 4 of the text book
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 7 (4.2)
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m,2))))

  def variance1(xs: Seq[Double]): Option[Double] = for {
    m <- mean(xs)
    y <- mean(xs.map(x => math.pow(x - m, 2)))
  } yield y


  // Exercise 8 (4.3)
  def map2[A, B, C](ao: Option[A], bo: Option[B])(f: (A, B) => C): Option[C] =
    ao flatMap(aa => bo map (bb => f(aa, bb)))

  def map2for[A, B, C](ao: Option[A], bo: Option[B])(f: (A, B) => C): Option[C] = for {
    aa <- ao
    bb <- bo
  } yield f(aa, bb)

  // Exercise 9 (4.4)
  def sequence[A](aos: List[Option[A]]): Option[List[A]] =
    List.foldRight(aos, Some(Nil): Option[List[A]])((x, y) => map2(x, y)(Cons(_, _)))

  // Exercise 10 (4.5)
  def traverse[A, B](aos: List[A])(f: A => Option[B]): Option[List[B]] =
    List.foldRight(aos, Some(Nil): Option[List[B]])((x, y) => map2(f(x), y)(Cons(_, _)))

}


// Test cases for running in the compiled vesion (uncomment as you go, or paste
// them into REPL in the interactive version)

object Tests extends App {

  // Exercise 1
  // val p = new java.awt.Point(0,1) with OrderedPoint
  // val q = new java.awt.Point(0,2) with OrderedPoint
  // assert(p < q)

  // Notice how we are using nice infix comparison on java.awt
  // objects that were implemented way before Scala existed :) (And without the
  // library implementing a suitable comparator). We did not have to recompile
  // java.awt.Point


  // Exercise 2
  // assert (Tree.size (Branch(Leaf(1), Leaf(2))) == 3)
  // Exercise 3
  // assert (Tree.maximum (Branch(Leaf(1), Leaf(2))) == 2)
  // Exercise 4
  // val t4 = Branch(Leaf(1), Branch(Branch(Leaf(2),Leaf(3)),Leaf(4)))
  // val t5 = Branch(Leaf("1"), Branch(Branch(Leaf("2"),Leaf("3")),Leaf("4")))
  // assert (Tree.map (t4) (_.toString) == t5)

  // Exercise 5
  // assert (Tree.size1 (Branch(Leaf(1), Leaf(2))) == 3)
  // assert (Tree.maximum1 (Branch(Leaf(1), Leaf(2))) == 2)
  // assert (Tree.map1 (t4) (_.toString) == t5)

  // Exercise 6
  // assert (Some(1).map (x => x +1) == Some(2))
  // assert (Some(41).getOrElse(42) == 41)
  // assert (None.getOrElse(42) == 42)
  // assert (Some(1).flatMap (x => Some(x+1)) == Some(2))
  // assert ((None: Option[Int]).flatMap[Int] (x => Some(x+1)) == None)
  // assert (Some(42).filter(_ == 42) == Some(42))
  // assert (Some(41).filter(_ == 42) == None)
  // assert ((None: Option[Int]).filter(_ == 42) == None)

  // Exercise 7
  // assert (ExercisesOption.variance (List(42,42,42)) == Some(0.0))
  // assert (ExercisesOption.variance (List()) == None)


  // Exercise 8
  // assert (ExercisesOption.map2 (Some(42),Some(7)) (_ + _) == Some(49))
  // assert (ExercisesOption.map2 (Some(42),None) (_ + _) == None)
  // assert (ExercisesOption.map2 (None: Option[Int],Some(7)) (_ + _) == None)
  // assert (ExercisesOption.map2 (None: Option[Int],None) (_ + _) == None)

  // Exercise 9
  // assert (ExercisesOption.sequence (List(Some(1), Some(2), Some(42))) == Some(List(1,2,42)))
  // assert (ExercisesOption.sequence (List(None,    Some(2), Some(42))) == None)
  // assert (ExercisesOption.sequence (List(Some(1), None,    Some(42))) == None)
  // assert (ExercisesOption.sequence (List(Some(1), Some(2), None    )) == None)

  // Exercise 10
  // def f (n: Int) :Option[Int] = if (n%2 == 0) Some(n) else None
  // assert (ExercisesOption.traverse (List(1,2,42)) (Some(_)) == Some(List(1,2,42)))
  // assert (ExercisesOption.traverse (List(1,2,42)) (f) == None)

}