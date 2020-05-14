package lectures.objectoriented

import scala.language.postfixOps

/**
 * In this component we will go through the scala syntactic sugar when creating methods. We will see how infix, prefix,
 * and postfix method notations would work in scala. We will also see apply() method in action.
 */
object FunctionNotation extends App {

  class Person(val name: String, val favMovie: String, val age: Int = 0) {
    def +(anotherPerson: Person): String = s"$name loves to hang out with ${anotherPerson.name}"

    def +(nickname: String): Person = new Person(s"$name ($nickname)", favMovie)

    def unary_+ : Person = new Person(name, favMovie, age + 1)

    def learns(msg: String): String = s"I am learning $msg."

    def learnScala: String = learns("scala")

    def apply(): String = "Hi, let's test the apply method()."

    def apply(watchCount: Int): String = s"$name has watched the movie $favMovie $watchCount times."
  }

  val mary: Person = new Person("Mary", "Bahubali")
  val anotherPerson: Person = new Person("Sharma", "Bahubali")

  // Infix notation
  println(mary + anotherPerson)
  // Prefix Notation
  println((+mary).age)
  // Postfix Notation
  println(mary learnScala)
  // Usage of apply
  println(mary())

  // Now some exercises.

  println((mary + "Rajni-Anna").apply(10))
  println(mary(15))

  /*
  And here is the output to the above program:
  Mary loves to hang out with Sharma
1
I am learning scala.
Hi, let's test the apply method().
Mary (Rajni-Anna) has watched the movie Bahubali 10 times.
Mary has watched the movie Bahubali 15 times.

   */
}
