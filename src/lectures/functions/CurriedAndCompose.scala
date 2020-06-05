package lectures.functions

object CurriedAndCompose extends App {
  // Implement a function toCurry(f: (Int,Int) => Int) => (Int => Int => Int)
  def toCurry(f: (Int, Int) => Int): (Int => Int => Int) = {
    x => y => f(x, y)
  }

  // Implement a function fromCurry(f: (Int => Int => Int)) => (Int,Int) => Int)
  def fromCurry(f: (Int => Int => Int)): (Int, Int) => Int = {
    (x, y) => f(x)(y)
  }

  // Implement a compose function (f,g) => x => f(g(x))
  def compose(f: Int => Int, g: Int => Int): Int => Int = {
    x => f(g(x))
  }

  // Implement an andThen function (f,g) => x => g(f(x))
  def andThen[A, B, T](f: A => B, g: B => T): A => T = {
    x => g(f(x))
  }

  println(toCurry((x, y) => x + y)(5)(6))
  println(fromCurry(x => y => x + y)(5, 6))
  println(compose(x => x + 2, y => y - 5)(4))
  println(andThen((x: Int) => x + 2, (y: Int) => y - 5)(4))

  val add2 = (x: Int) => x + 2
  val times5 = (y: Int) => y * 5
  val composed = compose(add2,times5)
  val ordered = andThen(add2,times5)

  println(composed(4))
  println(ordered(4))
}
