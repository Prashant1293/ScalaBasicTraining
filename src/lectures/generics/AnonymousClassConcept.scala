package lectures.generics

/**
 * Here we are going to see the concept of anonymous classes in scala.
 */
abstract class AnonymousClassConcept {
  def greet(name: String): String
}

object AnonymousEffect extends App {

  // Here, an object of `AnonymousClassConcept` is created using new Operator, but that class was an abstract class?
  // We cannot instantiate an abstract class. This is where the concept of anonymous classes comes into the picture.
  // When we create an object here using the new operator, technically it acts as a new operation on an anonymous class.
  val anonymousObj = new AnonymousClassConcept {
    override def greet(name: String): String = s"Hello $name, greetings from anonymous class."
  }
  println(anonymousObj.greet("Prashant"))

  // Let's see if we create another object with Anonymous class concept....

  val anotherObj = new AnonymousClassConcept {
    override def greet(name: String): String = s"Hello $name, greetings from another object."
  }

  println(anotherObj.greet("Sharma"))

  // Now let us try to print the 2 objects and see what the O/p is...

  println(s"anonymousObj = $anonymousObj, and anotherAnonymousObj = $anotherObj.")
  // anonymousObj = lectures.generics.AnonymousEffect$$anon$1@7bb11784,
  // and anotherAnonymousObj = lectures.generics.AnonymousEffect$$anon$2@33a10788.
  // Look at the parts {$$anon$2@33a10788 , $$anon$1@7bb11784}. This clears that the objects created were not
  // that of the abstract class, but were of some anonymous class that extends our abstract class.
}