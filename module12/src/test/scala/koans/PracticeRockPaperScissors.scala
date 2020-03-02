package koans

import scala.annotation.tailrec
import scala.util.control.TailCalls._


object Answer extends Enumeration {
  val Win, Loose, Tie = Value
}

//Step 1 Create an ADT to retry and finish
sealed trait Bounce1[+A]
case class Done1[A](item : A) extends Bounce1[A]
case class Call1[A](nextCall : () => Bounce1[A]) extends Bounce1[A]



object PracticeRockPaperScissors {

  @tailrec
  def trampoline[A](bounce: Bounce1[A]) : A = {
    bounce match {
      case done : Done1[A] => done.item
      case call1: Call1[A] => trampoline(call1.nextCall())
    }
  }

  def rock(i : Int) : Bounce1[Answer.Value] = {
    i match {
      case 0 => Done1(Answer.Tie)
      case x => Call1(() => paper(x - 1))
    }
  }

  def paper(i : Int) : Bounce1[Answer.Value] = {
    i match {
      case 0 => Done1(Answer.Win)
      case x => Call1(() => scissors(x - 1))
    }
  }

  def scissors(i : Int) : Bounce1[Answer.Value] = {
    i match {
      case 0 => Done1(Answer.Loose)
      case x => Call1(() => rock(x - 1))
    }
  }
}

object RockPaperScissorsTailRec extends TailRec[Answer.Value] {
  def rock(i : Int) : TailRec[Answer.Value] = {
    case 0 => done(Answer.Tie)
    case x => tailcall()
  }

  def paper()
}


object RockPaperScissors extends App {

  val practice = PracticeRockPaperScissors

  practice.trampoline(practice.rock(10))

}