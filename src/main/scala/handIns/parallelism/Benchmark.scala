package handIns.parallelism

import java.util.concurrent._
import Par._

object Benchmark {

  def main(args: Array[String]): Unit = {
    val myList = 200000 to 500000

    time {
      val res_par = sum(myList)
      val es = Executors.newWorkStealingPool()
      res_par(es).get
    }

    time{sum_seq(myList)}
  }

  def time[A](a: => A): A = {
    val now = System.nanoTime
    val result = a
    val micros = (System.nanoTime - now) / 1000
    println("%d microseconds".format(micros))
    result
  }

  // Examples of implementations using the Par API
  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else if (ints.size <= 1000) Par.unit(sum_seq(ints))
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

  def sum_seq(ints: IndexedSeq[Int]): Int = {
    if (ints.size <= 1)
      ints.headOption getOrElse 0 else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum_seq(l) + sum_seq(r)
    }
  }

  // Called as "sum(1 to 40)(Executors.newWorkStealingPool()).get

}
