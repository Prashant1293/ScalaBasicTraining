package lectures.functions

class FunctionalList {

}

/**
 * Well in this particular list implementation we are going to update the predicate and transformer code to leverage the
 * Function[] capability of Scala. This kind of implementation moves our code away from Object oriented programming and
 * leads towards functional programming. The most benefited methods would be map, flatMap, and filter here.
 *
 * @tparam A
 */
abstract class GenericListFunctional[+A] {
  def head: A

  def tail: GenericListFunctional[A]

  // This defines B as a supertype of A
  def add[B >: A](item: B): GenericListFunctional[B]

  def isEmpty: Boolean

  def printElements: String

  // Uses polymorphism to print the list elements recursively
  override def toString: String = "[" + printElements + "]"

  def map[B](transformer: A => B): GenericListFunctional[B]

  // See how we replaced the trait predicate with A => Boolean as a Function[].
  def filter(predicate: A => Boolean): GenericListFunctional[A]

  def flatMap[B](transformer: A => GenericListFunctional[B]): GenericListFunctional[B]

  // concatenation defined.
  def ++[B >: A](list: GenericListFunctional[B]): GenericListFunctional[B]
}

// We will need to extend the abstract class by 2 components, 1 an object denoting empty list,
// and 2nd a class denoting non-empty list.

case object MyEmptyList extends GenericListFunctional[Nothing] {
  def head: Nothing = throw new NoSuchElementException

  def tail: GenericListFunctional[Nothing] = throw new NoSuchElementException

  // This defines B as a supertype of Nothing which is any possible type in the scala type system.
  def add[B >: Nothing](item: B): GenericListFunctional[B] = new ContentList(item, MyEmptyList)

  def isEmpty: Boolean = true

  // Since this is an empty list it doesn't have any elements.
  def printElements: String = ""

  def filter(predicate: Nothing => Boolean): GenericListFunctional[Nothing] = MyEmptyList

  def map[B](transformer: Nothing => B): GenericListFunctional[B] = MyEmptyList

  def flatMap[B](transformer: Nothing => GenericListFunctional[B]): GenericListFunctional[B] = MyEmptyList

  // An empty list concatenated with itself would ultimately yield the same list only.
  def ++[B >: Nothing](list: GenericListFunctional[B]): GenericListFunctional[B] = list
}

case class ContentList[+A](header: A, tailOb: GenericListFunctional[A]) extends GenericListFunctional[A] {
  def head: A = this.header

  def tail: GenericListFunctional[A] = tailOb

  def add[B >: A](item: B): GenericListFunctional[B] = new ContentList(item, this)

  def isEmpty: Boolean = false

  def printElements: String = {
    if (tailOb.isEmpty) "" + header
    else header + "," + tailOb.printElements
  }

  def filter(predicate: A => Boolean): GenericListFunctional[A] = {
    if (predicate(header)) new ContentList(header, tailOb.filter(predicate))
    else tailOb.filter(predicate)
  }

  def map[B](transformer: A => B): GenericListFunctional[B] = {
    new ContentList(transformer(header), tailOb.map(transformer))
  }

  def ++[B >: A](list: GenericListFunctional[B]): GenericListFunctional[B] = {
    new ContentList(header, tailOb ++ list)
  }

  // for flatmap we need to create a concatenation method(++) in the abstract class.
  def flatMap[B](transformer: A => GenericListFunctional[B]): GenericListFunctional[B] = {
    transformer.apply(header) ++ tailOb.flatMap(transformer)
  }
}

// To change it into Function we need to analyze what does it takes in and what it outputs. Predicate takes in a T and returns a Boolean everytime.
// We won't need the predicate trait anymore, wherever it was needed we'll replace that by T => Boolean
/*
trait MyPredicate[-T] {
  def test(elem: T): Boolean
}
*/

// We won't need the transformer trait anymore, wherever it was needed we'll replace that by A => B
/*
trait MyTransformer[-A, B] {
  def transform(elem: A): B
}
*/

object FunctionalListExtension extends App {
  val listOfInts = new ContentList(1, new ContentList(2, new ContentList(3, MyEmptyList)))
  val listOfStrings = new ContentList("hello", new ContentList("generics", new ContentList("in scala", MyEmptyList)))

  println(s"list of ints = $listOfInts \nand list of strings = $listOfStrings")

  /*And here is the o/p of the generic list implementation:
  list of ints = [1,2,3]
    and list of strings = [hello,generics,in scala]
*/

  // Let us try apply the filter, map, and flatMap methods to our list.

  println(s"Filtered list with even numbers = ${
    listOfInts.filter((elem: Int) => elem % 2 == 0).toString
  }")

  println(s"Applied map to get the square of elements in the list = ${
    listOfInts.map((elem: Int) => elem * elem).toString
  }")

  println(s"Applying flatmap on the list = ${
    listOfInts.flatMap(new Function1[Int, GenericListFunctional[Int]] {
      override def apply(elem: Int): GenericListFunctional[Int] = new ContentList(elem, new ContentList(elem + 1, MyEmptyList))
    }).toString
  }")

  // Let's see how making case class and case object helps in our case. Also, now we do not need to use new keyword to
  // create instance of class. Case classes have their own apply method that takes care of object creation.
  val anotherListOfInts = ContentList(1, new ContentList(2, new ContentList(3, MyEmptyList)))
  println(listOfInts == anotherListOfInts)

}
