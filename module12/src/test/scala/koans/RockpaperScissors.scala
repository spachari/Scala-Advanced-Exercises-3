package koans

object Outcome extends Enumeration {
  val Tie, Win, Lose = Value
}

//Step 1: Create an ADT with Bounce with two subtypes
//  1. Done to indicate it is Done
//  2. Call to indicate that it is not Done
import scala.annotation.tailrec

sealed trait Bounce[+A]
case class Done[A](result : A) extends Bounce[A]
case class Call[A](nextFunc : () => Bounce[A]) extends Bounce[A]


object StillDaft {
  def even(n : Int) : Bounce[Boolean] = {
    n match {
      case 0 => Done(true)
      case x => Call(() => odd(x - 1))
    }
  }

  def odd(n : Int) : Bounce[Boolean] = {
    n match {
      case 0 => Done(false)
      case x => Call(() => even(x - 1))
    }
  }
}

object RockPaperScissorsV1  {

  def rock(i : Int) : Bounce[Outcome.Value] = {
    i match {
      case 0 => Done(Outcome.Tie)
      case x => Call(() => paper(x - 1))
    }
  }

  def paper(i : Int) : Bounce[Outcome.Value] = {
    i match {
      case 0 => Done(Outcome.Win)
      case xs => Call(() => scissors(xs - 1))
    }
  }

  def scissors(i: Int) : Bounce[Outcome.Value] = {
   i match {
     case 0 => Done(Outcome.Lose)
     case xs => Call(() => rock(xs - 1))
   }
  }
}

object Test extends App {

  @tailrec
  def trampoline[A](bounce: Bounce[A]) : A = {
    bounce match {
      case Done(x) => x
      case Call(nextFunc) => trampoline(nextFunc())
    }
  }

  val rockPaperScissorsV1 = RockPaperScissorsV1
  println(trampoline(rockPaperScissorsV1.rock(10)))

  val stillDaft = StillDaft
  trampoline(stillDaft.even(10))
}
