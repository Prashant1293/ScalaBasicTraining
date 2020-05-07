package lectures.basic

/**
 * Recursive calls are made using the internal stack space by the JVM. This means, if we try to reserve quite a big
 * stack space it would lead to stack overflow exception. The key to avoid getting into such trouble is to use
 * TAIL RECURSION. Tail recursion is when we call a method recursively in the last statement of the method. Check the
 * FunctionsExercise.isPrime() method it is as well tail recursion and thus could work for pretty larger integers. We
 * can use the annotation @tailrec on the method that is supposed to be a tail recursion method.
 * Here we will try to do the following using tail recursion:
 * 1. Make Factorial TailRecursive.
 * 2. Make Fibonacci TailRecursive.
 * 3. Make Concatenate a string n time using TailRecursion.
 *
 * The trick when using tail recursion is to rely on the extra input variables we use to define tail recursive methods.
 * As in the case of tailed factorial implementation we simply returned the value of Ack. In such an approach stack is
 * not used by the jvm to store the recursive function call for result computation. Instead, at every recursive call an
 * expression is created which is evaluated only when we enter the recursion break condition.
 */
object Recursion extends App {

  def tailedFactorial(number: Int): BigInt = {
    @scala.annotation.tailrec
    def factorialHelper(number: Int, ack: BigInt): BigInt = {
      if (number == 0 || number == 1) {
        ack
      }
      else {
        factorialHelper(number - 1, number * ack)
      }
    }

    factorialHelper(number, 1)
  }

  // Even this would be able to be printed on the screen, while if you try this with the earlier implementation it
  // would have thrown an exception.
  println(tailedFactorial(2500))

  // 2. Fibonacci as tailed recursion.


}
