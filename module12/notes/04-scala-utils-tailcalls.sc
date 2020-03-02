//Scala TailCalls define a TailRec trait, which is type parameterised type.
//TailRec[Boolean] is what the result of the tailrec will be (in out case it is Boolean).

//The methods inside of it return  a TailRec[Boolean]. It is a bit of a nicer API. Instead of
//using Done and Call they have provided methods for
//
// done()
//
// and
//
// tailcall() (it is very clean too, it uses by-name functions no need for () => even(x - 1))

import scala.util.control.TailCalls._

object StillDaft extends TailRec[Boolean] {
  def even(n : Int) : TailRec[Boolean] = {
    n match {
      case n if n == 0 => done(true)
      case n => tailcall(odd(n - 1))
    }
  }

  def odd(n : Int) : TailRec[Boolean] = {
    n match {
      case n if n == 0 => done(false)
      case n => tailcall(even(n - 1))
    }
  }
}

//Another advantage is the trampoline is built into the TailRec, we dont have to write it ourselves.
//SO when you run it,

StillDaft.even(6)

// if we say even of 6, we will end up with a tailcall of it

StillDaft.odd(6).result
//we need to say tailcall.result that will unwind the looping

StillDaft.even(999999).result
StillDaft.odd(999999).result

