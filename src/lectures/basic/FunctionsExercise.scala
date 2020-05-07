package lectures.basic

import scala.annotation.tailrec

/**
 * The Functions Exercise class helps understanding scala Functions, how to not use loops when calculating factorial,
 * fibonacci and prime numbers. This gives a better understanding of how programming in scala is different than Java.
 */

object FunctionsExercise extends App {

  def greetingFunction(name: String, age: Int): String = "Hello " + name + ", you are " + age + " years of age!"

  def factorial(number: Int): Int = {
    if (number == 0 || number == 1) {
      1
    }
    else number * factorial(number - 1)
  }

  def fibonacci(count: Int): Int = {
    if (count == 1 || count == 2) {
      1
    }
    else fibonacci(count - 1) + fibonacci(count - 2)
  }

  def isPrime(number: Int): Boolean = {
    @tailrec
    def checkPrime(num1: Int, num2: Int, divisor: Int): Int = {
      if (divisor > num2) {
        1
      }
      else if (num1 % divisor == 0) {
        0
      } else {
        checkPrime(num1, num2, divisor + 1)
      }
    }

    if (number == 0 || number == 1) {
      false
    } else if (number == 2) {
      true
    } else {
      if (checkPrime(number, number / 2, 2) != 0) {
        true
      } else {
        false
      }
    }
  }

  println(greetingFunction("Prashant", 26))
  println(factorial(6))
  println(fibonacci(8))
  println(isPrime(37))
  println(isPrime(2003))
}
