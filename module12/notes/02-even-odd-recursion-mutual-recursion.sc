//What about mutual recursion?

//Sometimes things are so neat. What about a -> b -> a -> b?

//Eg. Establish if a number is even or odd
//(it actually wont work with tail call optimization. That's because it is mutual recursion.
// THis is where a function does not call itself. It thenc alls another function, which then calls
// this one again, they flip flop between them)

//Scala's tail call recursion will not detect this case.

//Start with even1(n)

  //Calls odd1(n - 1)

  //Calls even1(n - 2)

//Until 0 is reached in either odd1 or even1
  //even1(0) is true
  //off1(0) is false (0 is even)

//A bit made up bit it explains for a simple example

object Daft {
  def even1(i : Int) : Boolean = {
    i match {
      case 0 => true
      case n => odd1(n - 1)
    }
  }

  def odd1(i : Int) : Boolean = {
    i match {
      case 0 => false
      case n => even1(n - 1)
    }
  }
}

Daft.even1(6)
Daft.odd1(6)

Daft.even1(999999)

//We will never create such a function, but it will tell us the idea that we are trying to get across here.
//1. Daft.even1(999999) - is a little bit slower
//2. Daft.even1(999999) - it throws a stack overflow

//let's see how we can fix this problem



