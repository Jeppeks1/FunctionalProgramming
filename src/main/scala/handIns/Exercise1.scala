package handIns

// Advanced Programming, Exercise1
import scala.annotation.tailrec

object Exercise1 extends App {

  // Exercise 3

  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, cur: Int): Int = {
      if (n == 0) 0
      else go(n - 1, cur, prev + cur)
    }

    go(n, 0, 1)
  }

  // Exercise 4

  // A simple object describing a cost line; implemented imperatively, Java
  // style (this way until we learn more Scala)
  class Expense {

    // A constructor definition
    def this (tag: String, price: Int) = {
      this()
      this.tag = tag
      this.price = price
    }

    var tag: String = "" // a tag line in the accounting system
    var price: Int  = 0 // the price is in cents
  }

  // computes the total of expenses in cents

  def total(expenses: Array[Expense]): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = {
      if (n < 0) acc
      else go(n - 1, acc + expenses(n).price)
    }

    go(expenses.length, 0)
  }

  val testcase1 = Array[Expense](
    new Expense("Coffee", 450),
    new Expense("Cake", 350) )


  // Exercise 5

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def go(n: Int, a: A, b: A): Boolean = {
      if (n >= as.length) true
      else if (ordered(a,b)) go(n+1, b, as(n+1)) else false
    }

    go(1, as(0), as(1))
  }

  // some tests (uncomment)

   assert ( isSorted (Array(1,2,3,4,5,6), (a: Int, b: Int) => a <= b))
   assert (!isSorted (Array(6,2,3,4,5,6), (a: Int, b: Int) => a <= b))
   assert (!isSorted (Array(1,2,3,4,5,1), (a: Int, b: Int) => a <= b))

  // Exercise 6

   def curry[A,B,C](f: (A, B) => C): A => B => C =
     a => b => f(a,b)

   def isSorted1[A]: Array[A] => ((A, A) => Boolean) => Boolean = curry(isSorted)

  // Exercise 7

   def uncurry[A,B,C](f: A => B => C): (A, B) => C =
     (a, b) => f(a)(b)

   def isSorted2[A]: (Array[A], (A,A) => Boolean) => Boolean = uncurry(isSorted1)

  // Exercise 8

  def compose[A,B,C](f: B => C, g: A => B) : A => C =
    a => f(g(a))
}
