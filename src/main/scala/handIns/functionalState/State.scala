package handIns.functionalState

import State._

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
}


object State {


  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // Exercise 9 (6.10) continued
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  type Rand[A] = State[RNG, A]

  // Called as "random_int.run(Simple(42))"
  def random_int: Rand[Int] = State(_.nextInt)

  // Exercise 10
  def state2stream[S, A](s: State[S, A])(seed: S): Stream[A] =
    s.run(seed) match {
      case (n, s1) => Stream.cons(n, state2stream(s)(s1))
    }


  // Exercise 11
   val random_integers: Stream[Int] = state2stream(random_int)(RNG.Simple(42))

}




















