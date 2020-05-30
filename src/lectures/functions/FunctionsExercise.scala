package lectures.functions

/**
 * The aim here is to work with functions as first class objects in scala.
 * 1. Write a function that takes 2 strings and concatenate them.
 * 2. Update the transformer and predicate in the generic list program to now leverage the function concept.
 * 3. Write a function that takes 1 int parameter and returns a function that takes an int and returns an int.
 */
object FunctionsExercise extends App {
  // Ques-1 solution:
  val concatenation: Function2[String, String, String] = new Function2[String, String, String] {
    override def apply(v1: String, v2: String): String = v1 + v2
  }

  println(concatenation("Hello ", "Scala World!"))

  // All Scala Functions are Objects, Scala supports function types up to 22 parameters.
  // Here's the syntactic sugar for using Function notation, this will be widely used in the scala code best practices.
  // To convert in syntactic sugar switch the expressions: [A, B, R] to (A, B) => R and the compiler would treat it as a Function2 notation.

  val syntacticConcat: (String, String) => String = (v1: String, v2: String) => v1 + v2

  println(syntacticConcat("Syntactic sugar:: Hello ", "Scala World!"))

  // For answer to Ques-2 go to the FunctionalList class under the functions package.

  // Ques-3 solution:
  val funOfFunTraditional: Function1[Int, Function1[Int, Int]] = new Function[Int, Function1[Int, Int]] {
    override def apply(v1: Int): Function1[Int, Int] = new Function[Int, Int] {
      override def apply(v2: Int): Int = v1 + v2
    }
  }
  println(s"Traditional way of creating curried function resulted in ${funOfFunTraditional(50)(40)}.")

  // This is also known as curried function.
  val funOfFun: Int => Int => Int = (v1: Int) => (v2: Int) => v1 + v2

  println(s"This shows how we can create a chain of Functions in scala as first class objects: funOfFun(4)(5) = ${funOfFun(4)(5)}.")

}
