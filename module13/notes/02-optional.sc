//The easiest way to convince us that Options is easy is to construct our own Option
//and write it. Admittedly, it is a super simple case. Option is as simple as it gets
//It is a container with very uncomplicated and straight forward semantics.
//
// It is like a box with Either something in it or without anything in it. It either got an item in or it does not
//we can define our Optional like this and define what it is to do the operations on an Empty box or a full box
//and once we have done that, we would have created a fully functional version of Optional (the equivalent of Option)
//and we can use it exactly the same way

//Note:
//We are using ADT
//This is less complete than, but similar to, the implementation of Option in the Scala standard library

sealed abstract trait Optional[+T] { //Optional is co-variant in type T
  def isDefined : Boolean            //This method is not needed for it to be a functor or a Monad it is just for convenience (we can call this and find out whether something is in the box or there isn't)
  def map[U](f : T => U) : Optional[U]         //For some new generic type U, a function : T => U, if we start with an Optional[T] we will get back an Optional[U]
  def flatMap[U](f : T => Optional[U]) : Optional[U]
                                     //The flatMap  (like a map and a flatten combined) is that, for some type U, a function : T => Optional[U], we will still get back an Optional[U]

}

//This is important, if we did a map over this and use a function : T => Optional[U], we will end up with a Optional[Optional[U]], but when we do a flatMap
//it will remove one of the levels. So if we start with an Optional[T] and we have a function of T => Optional[U], we will still get an Optional[U]. This reduction
//of complexity is very important for the functional workflows that we are going to see

case object Nada extends Optional[Nothing] { //nada is our version of Nothing. Because Optional[+T] is co-variant, Optional[Nothing] can be used as bottom class of Optional[Anything else]
  override def isDefined: Boolean = false

  override def map[U](f: Nothing => U): Optional[U] = Nada
  //It extends Optional[Nothing] so our function f needs to take Nothing => U, because it is co-variance, this will be satisfied by any function of type  : T => U
  //because of the contravariant nature of the function input types. It will just give Nada, it does not do anything with the input

  override def flatMap[U](f: Nothing => Optional[U]): Optional[U] = Nada
  //Same as above, if we start with empty box we will get back an empty box
}

case class Item[+T](value : T) extends Optional[T] { //Item is a field box, it is covariant in type T, the value is T,
  override def isDefined = true

  override def map[U](f: T => U) : Optional[U] = Item(f(value))
  //Here, we have value (value : T) We have the function : T => U, we need to get back a : Optional[U], so what we do is we say "if the box was full to begin with and we have
  // a function of type f: T => U, whenever we apply that function we will end up with a full box with a new item in it". SO we just wrap the result of the function on item
  //f(value) on Item. Like this Item(f(value)). We just call the Item with the result of the function call f on value

  override def flatMap[U](f: T => Optional[U]) : Optional[U] = f(value)
  //we already have a function of signature T => Optional[U] and what we need is an Optional[U]. Infact flatMap in this case is super simple. In this case, we already have a
  //function of type T => Optional[U] and we need Optional[U]. SO we simply apply the function on value. So the function takes the T and gives back Optional[U].
}

//Notes on the methods
//isDefined is a common pattern we will see in a lot of functional patterns/implementations. This is just a simple constant definition
//Gives false for Nada (indicating there is nothing in the box) and true for Item (indicating there is something in the box)
//It is a constant time operation. It works very fast (List use this)

//In this case, flatMap is simple but that is not always the case. THe most important this is for any implementation of one of these, we follow the type signature
//and we need to follow some rules

//Let's use the Optional type with Address
case class Address(street : String, ciry : String, state : String, zipCode : String)
case class Person(firstName : String, lastName : String, address : Optional[Address])

def zipCodeForPerson(person: Optional[Person]) : Optional[String] = {
  for {
    op <- person
    address <- op.address
  } yield address.zipCode
}

//This works exactly like how the Option type

val  people : List[Optional[Person]] = List(
  Item(Person("Fred", "Bloggs", Nada)),
  Item(Person("Simon", "Jones", Item(Address("123 Main", "Fakesville", "AZ", "12345")))),
  Nada
)

//Note if we ever see List[Optional[Person with Product with Serializable]],
// it is because the Item and Nada are case defined items they also implement Product and Serializable
// if we dont want to see them inferred, just extend the sealed class extend those
// sealed abstract trait Optional[+T] extend Product with Serializable {

//we will quite often see ADTs doing this because they are entirely implemented by case objects or classes
//it gets rid of that ugly inference 
people.map(x => zipCodeForPerson(x))
