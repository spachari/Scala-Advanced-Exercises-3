
//Higher Order Functions Recap

//First Order Functions are functions that take and return scalar values
val i = 10
val list = List(1,2,3,3)

//Higher Order Functions are functions that take, and/or, return function values
val func1 : (Int) => (Int) = (x : Int) => x * 100

list.map( x => func1(x)) //higher order functions taking a function

//Most of the times we have only seen these taking higher order functions

def hof ( i : Int)(f : Int => Int) : Int = {
  f(i)
}

hof(10)_
//For much of functional programming, we will also cover functions that return function values


//The ability to return an unapplied function is very powerful

//Combined with trampolines, the effect can be like an "interpreter" inside your program
import scala.annotation.tailrec
sealed trait Bounce[A]
case class Done[A](result : A) extends Bounce[A]
case class Call[A](nextFunc : () => Bounce[A]) extends Bounce[A]

@tailrec
def trampoline[A](bounce : Bounce[A]) : A = {
  bounce match {
    case Call(nextFunc) => trampoline(nextFunc())
    case Done(x) => x
  }
}

//This is interpreting two different states. either a Call or a Done. We decided how it is run
//it can choose how it can be executed (like send it to other machines). If you think ahead, we might
//see more than one kind of interpreter
