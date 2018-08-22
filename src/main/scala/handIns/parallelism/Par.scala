package handIns.parallelism

import java.util.concurrent._
import scala.language.implicitConversions


object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(
    new Callable[A] {
      def call: A = a(es).get
    }
  )

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = as.map(asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  def map3[A, B, C, D](pa: Par[A], pb: Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] =
    (es: ExecutorService) => {
      val af = pa(es)
      val bf = pb(es)
      val cf = pc(es)
      UnitFuture(f(af.get, bf.get, cf.get))
    }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val ind = run(es)(n).get
      run(es)(choices(ind))
    }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(a => if (a) 0 else 1))(List(t, f))


  // Exercise 6 (CB7.13)
  def flatMap[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val ind = run(es)(pa).get
      run(es)(choices(ind))
    }

  def choiceNFlatMap[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(n)(choices)

  def choiceFlatMap[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond)(a => if (a) t else f)

  def join[A](a: Par[Par[A]]): Par[A] = es => {
    run(es)(run(es)(a).get())
  }


  class ParOps[A](p: Par[A]) {

  }

  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)
}

