package handIns.lense

import handIns.functionalState.State

case class LensT[R, F](get: R => F, set: (R, F) => R) {
  def modify(l: LensT[R, F])(f: F => F): R => R =
    r => l set(r, f(l get r))

  def compose[Q](g: LensT[Q, R]): LensT[Q, F] =
    LensT(
      get = get compose g.get,
      set = (q, f) => g set(q, set(g get q, f))
    )

  def |||[S, G](y: LensT[S, F]): LensT[Either[R, S], F] =
    LensT(
      get = {
        case Left(r) => get(r)
        case Right(s) => y get s
      },
      set = {
        case (Left(r), f) =>
          Left(set(r, f))
        case (Right(s), f) =>
          Right(y.set(s, f))
      })

  def +=(n: F)(implicit m: Numeric[F]): State[R, F] =
    State(r => {
      val w = m.plus(get(r), n)
      (w, set(r, w))
    })

  def :=(f: => F): State[R, F] =
    State(r => (f, set(r, f)))

}


case class CoState[F, R](get: F, set: F => R) {
  def map[S](f: R => S): CoState[F, S] =
    CoState(
      get = get,
      set = f compose set
    )

  def coFlatMap[S](f: CoState[F, R] => S): CoState[F, S] =
    CoState(
      get = get,
      set = k => f(CoState(k, set))
    )
}

case class PartialLens[T, C](apply: T => Option[CoState[C, T]]) {

  // Partial lens category composition
  def compose[D](f: PartialLens[C, D]): PartialLens[T, D] =
    PartialLens(t => for {
      c <- this apply t
      d <- f apply c.get
    } yield CoState(
      d.get
      , x => c set (d set x)
    ))

  // Alternate on a choice
  def |||[U](f: PartialLens[U, C]): PartialLens[Either[T, U], C] =
    PartialLens {
      case Left(a) =>
        apply(a) map (x => CoState(
          x.get
          , w => Left(x set w)
        ))
      case Right(b) =>
        f apply b map (y => CoState(
          y.get
          , w => Right(y set w)
        ))
    }

  // State value for monadic programming
  def state: State[T, Option[C]] =
    State(t => (apply(t) map (_.get), t))

  // Modify constructor values
  def modify(f: C => C): T => T =
    t => apply(t) match {
      case None => t
      case Some(w) => w.set(f(w.get))
    }
}

object PartialLens {
  def listHeadLensT[A]: PartialLens[List[A], A] =
    PartialLens[List[A], A]({
      case Nil => None
      case h :: t => Some(CoState(h, _ :: t))
    })

  def listTailLensT[A]: PartialLens[List[A], List[A]] =
    PartialLens[List[A], List[A]]({
      case Nil => None
      case h :: t => Some(CoState(t, h :: _))
    })

  def listNthLensT[A](n: Int): PartialLens[List[A], A] =
    if (n < 0)
      PartialLens(_ => None)
    else if (n == 0)
      listHeadLensT
    else
      listTailLensT compose listNthLensT(n - 1)
}


object test extends App{
  case class Address(street: Option[String], state: Option[String])
  case class Person(age: Option[Int], address: Option[Address])

  val target = Person(Some(55), Some(Address(None, Some("Denmark"))))

  val optionPA = new PartialLens[Person, Option[Address]] ({
    x => Some(CoState(x.address, y => x.copy(address = y)))
  })

  val optionAS = new PartialLens[Option[Address], Option[String]] ({
    case None => None
    case Some(a) => Some(CoState(a.state, y => Some(a.copy(state = y))))
  })

  val getState = optionPA.compose(optionAS).apply(target).get.get
  val newState = optionPA.compose(optionAS).apply(target).get.set(Some("Germany"))
  val modState = optionPA.compose(optionAS).modify(x => Some(x.get + " - Roskilde")).apply(target)

  println(getState)
  println(newState)
  println(modState)

  // -----------------------------------------------------------

  case class Address2(street: String, nr: Int)
  case class Person2(name: String, addresses: List[Address2])

  val target2 = Person2("Jeppe", List(Address2("Smørumvej", 68), Address2("Rådmandsgade", 47)))

  val optPA = new PartialLens[Person2, List[Address2]] ({
    x => Some(CoState(x.addresses, y => x.copy(addresses = y)))
  })

  val optAS = new PartialLens[Address2, String] ({
    x => Some(CoState(x.street, y => x.copy(street = y)))
  })

  val optComp = (optPA compose PartialLens.listNthLensT(1) compose optAS)
                .modify(_ => "Bernhards Bangsvej")
                .apply(target2)
  println(optComp)


}



