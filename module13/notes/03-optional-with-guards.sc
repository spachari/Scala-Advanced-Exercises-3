//We saw that all the generator get turned into flatMaps, when wehave a guard in there, these calls get turned into
//a call to withFilter which takes a predicate (p : T => Boolean) and give back Optional[T] and the idea here is that
//if the predicate fails, we can change the State of the container. Let's say in our case if the filter fails we want to end up with
//and empty box

//In the case of a list, we will remove it from the List, in case of a Future, it will become a Failure. This is the
//key to this kind of container-pattern. When we write the for-expression, we are assuming everything is working (this is
// the happy path). What the container is doing (Monad) is defining what failure will look like. It says 'Ok, when things are not
// working, here is the State that I want you to adopt'

//Let's see the implementation with withFilter (just for practice)

sealed abstract trait Optional[+T] extends Product with Serializable {
  def isDefined : Boolean
  def map[U](f : T => U) : Optional[U]  //Functor
  def flatMap[U](f : T => Optional[U]) : Optional[U] //Monad
  def withFilter(p : T => Boolean) : Optional[T] //guards
}

case object Nada extends Optional[Nothing] {
  override def isDefined = false

  override def map[U](f: Nothing => U) = Nada

  override def flatMap[U](f: Nothing => Optional[U]) = Nada

  override def withFilter(p: Nothing => Boolean) = Nada
}

case class Item[T](value : T) extends Optional[T] {
  override def isDefined = true

  override def map[U](f: T => U) : Optional[U] = Item(f(value))

  override def flatMap[U](f: T => Optional[U]) : Optional[U] = f(value)

  override def withFilter(p: T => Boolean) : Optional[T] = if (p(value)) this else Nada
}

//Notes
//withFilter - For the case of an item, if the predicate is true, we are going to persist with this Option
//we are going to have a Box with tis Item still in it. If the predicate is not true, we are going to go to the
//empty state (Box).

case class Address(street : String, ciry : String, state : String, zipCode : String)
case class Person(firstName : String, lastName : String, address : Optional[Address])

val people = List(
  Item(Person("Fred", "Bloggs", Nada)),
  Item(Person("Simon", "Jones", Item(Address("123 Main", "Fakesville", "AZ", "12345")))),
  Item(Person("Harry", "Potter", Item(Address("123 Buckingham Road", "Buckinghamville", "AZ", "12345")))),
  Nada
)

def zipForPerson(startsWith : String)(person: Optional[Person]) : Optional[String] = {
  for {
    op <- person
    if op.firstName.startsWith(startsWith)
    address <- op.address
  } yield address.zipCode
}

people.map(person => zipForPerson("S")(person))

val simon = Item(Person("Simon", "Jones", Item(Address("123 Main", "Fakesville", "AZ", "12345"))))
val fred = Item(Person("Fred", "Bloggs", Nada))

//Notice one thing, just comment out the line in 17 (the def withFilter in sealed abstract trait)

//we will get this error message
/*
Error:(57, 15) value withFilter is not a member of Optional[Person]
op <- person
 */

//becuase as soon as it hits the line,
// if op.firstName.startsWith(startsWith), it starts to look for the withfilter method in the sealed trait Optional

//Note:
//withFilter only needs to be there, if we are doing a guard or if we are doing pattern matching.
//But if we are only doing maps and flatMap we do not need withFilter

//wihtFilter is not just being a part of a Functor or Monad, it's part of being able to use guards or pattern matching
//in the for-expression in Scala. But then again there are rules and good documentation there on what
//withFilter should do and how it should behave to make a well behaved container to work inside of
//a for-expression

//Let's use Optional in pattern matching

def getValue(optional: Optional[Int]) : Int = {
  optional match {
    case optional : Item[Int] => optional.value
    case _ => 0
  }
}

getValue(Item(100))

//Functor laws