package handIns.functionalState

import State._

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(i => modify[Machine](stepMachine(i, _))))
    s <- get[Machine]
  } yield (s.candies, s.coins)

  def stepMachine(inputs: Input, s: Machine): Machine = (inputs, s) match {
    case (_, Machine(_, 0, _)) => s
    case (Coin, Machine(false, _, _)) => s
    case (Turn, Machine(true, _, _)) => s
    case (Coin, Machine(true, candy, coin)) =>
      Machine(locked = false, candy, coin + 1)
    case (Turn, Machine(false, candy, coin)) =>
      Machine(locked = true, candy - 1, coin)
  }
}

// Called as "simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(true, 5, 10))"
