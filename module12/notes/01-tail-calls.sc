//Recursion Vs Loops

//(Eventually our program is going to be compiled and going to run on a CPU
// At that point it is going to run very very imperative. So the whole functional
// programming approach of trying to remove loops and conditionals make it less clear that we are running
// it in imperative way are a fiction. because at some point they will get turned into a CPU which will run imperatively)

//But the idea is to put that point off as long as possible and to isolate our iterative approaches (side effect) in FP.

//Iteration is a key part of any imperative program

//Recursion offers a fully functional immutable alternative

def fact(n : Int) = {
  var i = 0
  var acc = 1

  while(i < n) {
    acc += i * acc
    println(acc + " " + i )
    i += 1
  }
  acc
}

fact(8)
//fact(9999999)

def factRecursion(n : Int, max : Int) : Int = {
  if (n <= max) n * factRecursion(n + 1, max) else 1
}

factRecursion(1, 8)

def factR(n : Int) : Int = {
  if (n > 0) n * factR(n - 1) else 1
}

factR(8)

def fact3(n : Int) : Int = {
  if (n < 2) n else n * fact3(n - 1)
}

fact3(8)


//Recursion is the practice of calling a function from another function. In particular self-recursion or tail-recursion
//is when the last thing called is actually the same method.

//Note: there is such thing as mutual recursion which is when methods call one another and flip back and forth

//Self recursion like this is just where one method is implemented in terms of itself.

/*
No vars, stack frames used to track state

//There is not while loop but we need a termination condition. Recursion that does not have a termination condition are infinite.
//There are things that come to stop that like stack limit being exceeded.

But stack frames can (and do) overflow

Scala has a trick for that
 */

//fact3(9999999)
//THis is because it is trying to recursively call this. Stacks on mordern computers are nowhere near to the heap.
//Stacks on modern languages are tipically nowhere near the heap so at some point we cross over and fill up the stack
//(the stack frames is filled up and memory is used up) at which point the run-time fails and we see the stackoverflow and
//we can see that in the error message

//How do we fix this?

//It turns out that scala has a trick for this. If the last call in a method (or in a particular branch in a method) is a call back
//to the function, it will do what is called a tail call optimization and it will turn this back into an actual while loop.

//This is not cheating, remember eventually it is going to running in a CPU and will be imperative. The point is we can write functional
//code that is clean and free of vars and loops and have scala give us this with the same efficiency as the while loop and the same benefits
//of nto blowing up the stack as in the before example

//In order to do this what we need to make sure is the call itself is the last call

//In the above example, we can see that

/*
def fact3(n : Int) : Int = {
  if (n < 2) n else n * fact3(n - 1)
}
 */

//this may look like fact3(n - 1) is the last call, but what happens is it calls fact3(n - 1), but then multiply it
//by n. SO the last call is n * fact3(n - 1). SO the * is the last call here.


@scala.annotation.tailrec
def fact4(n : Int, output : Long): Long = {
  if (n > 0) fact4(n - 1, output * n) else output
}

fact4(8, 1)
fact4(999999, 1)

//Here, the * is done before the method is called. So the last thing on the else is the call to that method.
//The other side of the condition just returns the value. This whole thing turns into a proper while loop

//Notice the difference between the icons for fact3 and fact4.

//The accumulator approach is a very common one to do this kind of work. We can do any work we like before
//we call the method (in our case it is *). This