case class Address(street : String, city : String, state : String, zipCode : String)
case class Person(firstName : String, lastName : String, address : Option[Address])

def zipForPerson(person: Option[Person]) : Option[String] = {
  for {p <- person
       address <- p.address} yield {address.zipCode}
}
//Note that there are two places the job can fail
//  1. Person may be None
//  2. Address may be None

//The idiomatic way of doing this in Scala is by using a for-expression

//This is a concept we would should to get used to. When we use a for-expression, we are saying what
//we would have liked to happen assuming that everything works out with these containers.
//
// A for-expression works over some kind of container (In this case an Option) and it says if there is
//for {p <- person // something there do this with it
//     address <- p.address} //then if there is something in p do this
//
// If any of these are None, it gives us back a None. All of the generators have to be over the same category of items
//If we start with an Option we should have Options all the way through. If we start with a collection, we
//should have a collection all the way through. Same with Futures (If you have Futures we will have Futures
// all the way through)

//THis code will not fail, if there is no person or there is no Address, it will simply yield up a None
//If there is a Person and Address, it will get us the zipcode

val people = List(
    Some(Person("Fred", "Bloggs", None)),
    Some(Person("Simon", "Jones", Some(Address("123 Main", "Fakesville", "AZ", "12345")))),
    None
)

people.map(person => zipForPerson(person))

val simon = Some(Person("Simon", "Jones", Some(Address("123 Main", "Fakesville", "AZ", "12345"))))
val fred = Some(Person("Fred", "Bloggs", None))

//here is how the for-expression gets evaluated. When we have a for-expression with multiple generators an yield,
//each of the generators becomes a flatMap except the last one, which becomes a map.

//if we want to know why the generators have to be the same type, let's write things like this (fred.flatMap(_.address).map(_.zipCode))
// and we will find out that if we start mixing Lists, Futures and Options and things together, very quickly we will see that the type
// signatures of map and flatMap are violated by mixing those types.

fred.flatMap(_.address).map(_.zipCode)
simon.flatMap(_.address).map(_.zipCode)

//So this is Option and we have used it all the way through. Now we can say that because of this and a
//couple of rules that it follows Option is a Functor and then flatMap and a couple of other things and a few that
//it follows makes it a Monad