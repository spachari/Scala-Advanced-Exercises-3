//ADTs Recap

//A type T with a controlled finite number of sub types.
//(This is a FP idea. ADTs are some type T at the top of a hierarchy with a tightly controlled finite set of sub-types
// When we say finite, there are certain types underneath it. THere can be all sort of states in there (like all of the Integers),
// but the point is, there is some types (3 or 4) and those are the only types that can ever extend it)

//In Scala, usually sealed trait (or sealed abstract class) with case class sub-types

import scala.annotation.tailrec
sealed trait Bounce[A]
case class Done[A](result : A) extends Bounce[A]
case class Call[A](nextFunc : () => Bounce[A]) extends Bounce[A]
//Note that nextFunc can be giving out another Bounce[A], it could be another Done or a Call

//We make a mini language with Done indicating that we are finished and Call indicating that here is more to do

//The top trait is called Bounce because we are going to Bounce off the top of the stack rather than descending into it
//(like a trampoline)
//(At each point a call to this function will return another Bounce but we wont call into the stack, but we can then
// get that Bounce back and we can then run a loop. When we run that we never descend into the Stack)

@tailrec
def trampoline[A](bounce : Bounce[A]) : A = {
  bounce match {
    case Call(nextFunc) => trampoline(nextFunc())
    case Done(x) => x
  }
}

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
//The method even now returns a Bounce[Boolean]
//in the case of 0, it is a Done(true)
//in the case of anything else it does a Call(() => odd(x - 1)) and note that it is a lazy function call, so it wont be evaluated right now.
//that is what makes it not descend the stack, it becomes a new

//at this level,
// Call(() => odd(x - 1))

//it becomes a new item, with a function () => odd(x - 1). That function will resolve in another Bounce

//This is a crucial concept in functional programming. Incidentally when we return a function, from another function from another method or function,
//that makes it a higher order function as well.

//Note: Higher order functions are not only function that take another functions, but they can be functions that return other functions
//Most of the functional programming patterns rely on these higher order functions that return other functions for work to be done.

//The idea is, here is the result, but that result requires more work to be done

StillDaft.even(6)
//Note that this function returns a function or a Bounce[]. that is fully of work needs to be done that needs to be done yet
//i.e. res0: Bounce[Boolean] = Call(StillDaft$<function>

//in simple words it is just calling odd

//it only happens when we call trampoline on it. trampoline gets that Bounce and starts unwinding it in a loop (it checks if it is a Call
// then Call(() => odd(x - 1)) is it a  Done, then I am finished).

trampoline(StillDaft.even(6))
trampoline(StillDaft.odd(6))
trampoline((StillDaft.odd(9999999)))
trampoline((StillDaft.even(9999999)))

//Even though it will not blow the stack, it is still not as efficient as a while loop. There is still some
//
// Call(() => even(x - 1)). i.e. Call.apply() happening there. But the point is atleast you can do this type of mutual recursion and you dont
//blow the stack and another very profound thing has happened now, we have actually split the execution into two placea.

//We have actually split the execution into two places.
//1. we got the part of this that says what it wants to do, in other words, call another Bounce with a call or done in it.

/*
  def even(n : Int) : Bounce[Boolean] = {
    n match {
      case 0 => Done(true)
      case x => Call(() => odd(x - 1))
    }
  }
 */

//2. and then we have the thing that does the work i.e. the trampoline
/*
@tailrec
def trampoline[A](bounce : Bounce[A]) : A = {
  bounce match {
    case Call(nextFunc) => trampoline(nextFunc())
    case Done(x) => x
  }
}
 */

//The trampoline, at any point, might decide it does nto want to call the next step. SO we have two places that are responsible for
//the execution. We got what we want to do and where it is actually done.

/*
Trampolines

Give Full Control Over the Execution
    E.g. Execution can be interrupted by the runner
    (We could actually choose to try with the other thing, for example, here is how i am going to call it. We could actually
    call it with Futures if we wanted to)

This implementation uses no vars at all (@tailrec)

Works via higher order functions, return is another function to execute

Potentially endless execution
(It is possible to make an infinite loop out of it and it will never blow up. will just keep running functions internally.
This will keep running functions internally. THis makes the whole approach turing complete. One of the necessary thing to ensure
turin completeness is that a machine should provide endless execution

Slower than iteration still, but pure FP

A little less readable than imperative sometimes

ADTs and trampolines are the building blocks of many functional patterns

Scala has its own implementation in scala.util.control.TailCalls
 */