package handIns.fingerTree

import scala.language.higherKinds

// The implementation is based on Section 3 of the paper.
//
// This implementation is designed to be eager, following the regular strictness
// of Scala.  However it would be an interesting exercise to extend it so that
// it is possibly lazy, like in the paper of Hinze and Paterson.  The obvious
// choice is to make values of elements stored in the queue lazy.  Then there is
// also a discussion of possible suspension of the middle element of the tree on
// page 7.

// I changed the type of reducers to not use curried operators, but regular
// binary operators.  This is more natural in Scala, and gives easier to read
// syntax of expressions.  Curried style is preferred in Haskell.

trait Reduce[F[_]] {
  def reduceR[A, B](opr: (A, B) => B)(fa: F[A], b: B): B

  def reduceL[A, B](opl: (B, A) => B)(b: B, fa: F[A]): B

  // page 3
  def toList[A](fa: F[A]): List[A] =
    reduceR[A, List[A]](_ :: _)(fa, List())

  // page 6
  def toTree[A](fa: F[A]): FingerTree[A] =
    reduceR[A, FingerTree[A]](FingerTree.addL)(fa, Empty())
}

// Types for Finger trees after Hinze and Pattersoni (page 4)


sealed trait Node[+A] {
  def toList: List[A] = Node.toList(this)
}

case class Node2[A](l: A, r: A) extends Node[A]
case class Node3[A](l: A, m: A, r: A) extends Node[A]

sealed trait FingerTree[+A] {

  // The following methods are convenience delagation so we can use
  // the operations both as methods and functions.
  // Uncomment them once you have implemented the corresponding functions.

  def addL[B >: A](b: B): FingerTree[B] = FingerTree.addL(b, this)

  def addR[B >: A](b: B): FingerTree[B] = FingerTree.addR(this, b)

  def toList: List[A] = FingerTree.toList(this)

  def headL: A = FingerTree.headL(this)

  def tailL: FingerTree[A] = FingerTree.tailL(this)

  def headR: A = FingerTree.headR(this)

  def tailR: FingerTree[A] = FingerTree.tailR(this)

  // page 7 (but this version uses polymorphis for efficiency, so we can
  // implement it differently; If you want to follow the paper closely move them to
  // FingerTree object and delegate the methods, so my tests still work.
  //
  def empty: Boolean = this match {
    case NilTree() => true
    case ConsL(_, _) => false
  }

  def nonEmpty: Boolean = !empty
}
case class Empty() extends FingerTree[Nothing] {

  // page 7
  //
  // override def empty =  ...
  // override def nonEmpty = ...
}
case class Single[A](data: A) extends FingerTree[A]

// paramter names: pr - prefix, m - middle, sf - suffix
case class Deep[A](pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A]) extends FingerTree[A]


// page 6
//
// Types of views on trees
// The types are provided for educational purposes.  I do not use the view
// types in my implementation. I implement views as Scala extractors.
// But you may want to implement views first like in the paper, and then
// conver them to Scala extractors.

// In the paper views are generic in the type of tree used. Here I make them
// fixed for FingerTrees.

//sealed trait ViewL[+A]
//case class NilTree() extends ViewL[Nothing]
//case class ConsL[A](hd: A, tl: FingerTree[A]) extends ViewL[A]

// Left extractors for Finger Trees (we use the same algorithm as viewL in the
// paper). You can do this, once you implemented the views the book way.
// Once the extractors are implemented you can pattern match on NilTree, ConsL
// and ConsR
//
// See an example extractor implemented for Digit below (Digit.unapply)

object NilTree { // we use the same extractor for both left and right views
  def unapply[A](t: FingerTree[A]): Boolean = t match {
    case Empty() => true
    case _ => false
  }
}

object ConsL {
  def unapply[A](t: FingerTree[A]): Option[(A, FingerTree[A])] = t match {
    case NilTree() => None
    case Single(x) => Some(x, Empty())
    case Deep(pr, m, sf) => Some(pr.head, FingerTree.deepL(pr.tail, m, sf))
  }
}


object ConsR {
  def unapply[A](t: FingerTree[A]): Option[(FingerTree[A], A)] = t match {
    case NilTree() => None
    case Single(x) => Some(Empty(), x)
    case Deep(sf, m, pr) => Some(FingerTree.deepR(sf.tail, m, pr), sf.head)
    // The paper talks about a ViewL consisting of a Cons(head, tail)
    // which is why this does not give the very last element in the structure.
  }
}


object Digit extends Reduce[Digit] {

  // page 3, top
  def reduceR[A, Z](opr: (A, Z) => Z)(d: Digit[A], z: Z): Z =
    d.foldRight(z)(opr)

  def reduceL[A, Z](opl: (Z, A) => Z)(z: Z, d: Digit[A]): Z =
    d.foldLeft(z)(opl)

  // Digit inherits toTree from Reduce[Digit] that we will also apply to other
  // lists, but this object is a convenient place to put it (even if not all
  // lists are digits)

  def apply[A](as: A*): Digit[A] = List(as: _*)

  // This is an example of extractor, so that we can use Digit(...) in pattern
  // matching.  Case classes have extractors automatically, but Digit defined
  // as above is not a case class, but just a type name.
  def unapplySeq[A](d: Digit[A]): Option[Seq[A]] = Some(d)
}


object Node extends Reduce[Node] {

  def reduceR[A, Z](opr: (A, Z) => Z)(n: Node[A], z: Z): Z = n match {
    case Node2(a, b) => opr(a, opr(b, z))
    case Node3(a, b, c) => opr(a, opr(b, opr(c, z)))
    case _ => z
  }

  def reduceL[A, Z](opl: (Z, A) => Z)(z: Z, n: Node[A]): Z = n match {
    case Node2(b, a) => opl(opl(z, b), a)
    case Node3(c, b, a) => opl(opl(opl(z, c), b), a)
    case _ => z
  }

}


// Most of the paper's key functions are in the module below.

object FingerTree extends Reduce[FingerTree] {

  // page 5
  def reduceR[A, Z](opr: (A, Z) => Z)(t: FingerTree[A], z: Z): Z = t match {
    case Empty() => z
    case Single(x) => opr(x, z)
    case Deep(pr, m, sf) => Digit.reduceR(opr)(pr, FingerTree.reduceR(Node.reduceR(opr))(m, Digit.reduceR(opr)(sf, z)))
  }

  def reduceL[A, Z](opl: (Z, A) => Z)(z: Z, t: FingerTree[A]): Z = t match {
    case Empty() => z
    case Single(x) => opl(z, x)
    case Deep(pr, m, sf) => Digit.reduceL(opl)(FingerTree.reduceL(Node.reduceL(opl))(Digit.reduceL(opl)(z, pr), m), sf)
  }

  // page 5 bottom (the left triangle); Actually we could use the left
  // triangle in Scala but I am somewhat old fashioned ...

  def addL[A](a: A, t: FingerTree[A]): FingerTree[A] = t match {
    case Empty() => Single(a)
    case Single(b) => Deep(Digit(a), Empty(), Digit(b))
    case Deep(Digit(b, c, d, e), m, sf) => Deep(Digit(a, b), addL(Node3(c, d, e), m), sf)
    case Deep(pr, m, sf) => Deep(a :: pr, m, sf)
  }

  def addR[A](t: FingerTree[A], a: A): FingerTree[A] = t match {
    case Empty() => Single(a)
    case Single(b) => Deep(Digit(b), Empty(), Digit(a))
    case Deep(pr, m, Digit(e, d, c, b)) => Deep(pr, addR(m, Node3(e, d, c)), Digit(b, a))
    case Deep(pr, m, sf) => Deep(pr, m, sf ++ Digit(a))
  }

  // page 6
  //
  // This is a direct translation of view to Scala. You can replace it later
  // with extractors in Scala, see above objects NilTree and ConsL (this is an
  // alternative formulation which is more idiomatic Scala, and slightly
  // better integrated into the language than the Haskell version).
  // In Haskell we need to call viewL(t) to pattern match on views.  In Scala,
  // with extractors in place, we can directly pattern match on t.
  //
  // def viewL[A] (t: FingerTree[A]) :ViewL[A] = ...

  // page 6

  // A smart constructor that allows pr to be empty
  def deepL[A](pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A]): FingerTree[A] = pr match {
    case List() => m match {
      case NilTree() => Digit.toTree(sf)
      case ConsL(a, r) => Deep(a.toList, r, sf)
    }
    case _ => Deep(pr, m, sf)

  }

  // A smart constructor that allows sf to be empty
  def deepR[A](sf: Digit[A], m: FingerTree[Node[A]], pr: Digit[A]): FingerTree[A] = sf match {
    case List() => m match {
      case NilTree() => Digit.toTree(pr)
      case ConsR(l, a) => Deep(a.toList, l, pr)
    }
    case _ => Deep(sf, m, pr)
  }

  // page 7

  def headR[A](t: FingerTree[A]): A = t match {
    case ConsR(_, a) => a
  }

  def headL[A](t: FingerTree[A]): A = t match {
    case ConsL(a, _) => a
  }

  def tailL[A](t: FingerTree[A]): FingerTree[A] = t match {
    case ConsL(_, x) => x
  }

  def tailR[A](t: FingerTree[A]): FingerTree[A] = t match {
    case ConsR(x, _) => x
  }




}