package lectures.generics.advanced

/**
 * We have already created a generic implementation of a user defined list. What is left now is to understand
 * the anonymous classes in scala. With the help of these classes we will be able to create the other special methods
 * on the list instance that are map, flatmap, & filter. Let's go through the implementation.
 */
class AnonymousClassesAndList {

}

abstract class GenericList[+A] {
  def head: A

  def tail: GenericList[A]

  // This defines B as a supertype of A
  def add[B >: A](item: B): GenericList[B]

  def isEmpty: Boolean

  def printElements: String

  // Uses polymorphism to print the list elements recursively
  override def toString: String = "[" + printElements + "]"

  def map[B](transformer: MyTransformer[A, B]): GenericList[B]

  def filter(predicate: MyPredicate[A]): GenericList[A]

  def flatMap[B](transformer: MyTransformer[A, GenericList[B]]): GenericList[B]

  // concatenation defined.
  def ++[B >: A](list: GenericList[B]): GenericList[B]
}

// We will need to extend the abstract class by 2 components, 1 an object denoting empty list,
// and 2nd a class denoting non-empty list.

object MyEmptyList extends GenericList[Nothing] {
  def head: Nothing = throw new NoSuchElementException

  def tail: GenericList[Nothing] = throw new NoSuchElementException

  // This defines B as a supertype of Nothing which is any possible type in the scala type system.
  def add[B >: Nothing](item: B): GenericList[B] = new ContentList(item, MyEmptyList)

  def isEmpty: Boolean = true

  // Since this is an empty list it doesn't have any elements.
  def printElements: String = ""

  def filter(predicate: MyPredicate[Nothing]): GenericList[Nothing] = MyEmptyList

  def map[B](transformer: MyTransformer[Nothing, B]): GenericList[B] = MyEmptyList

  def flatMap[B](transformer: MyTransformer[Nothing, GenericList[B]]): GenericList[B] = MyEmptyList

  // An empty list concatenated with itself would ultimately yield the same list only.
  def ++[B >: Nothing](list: GenericList[B]): GenericList[B] = list
}

class ContentList[+A](header: A, tailOb: GenericList[A]) extends GenericList[A] {
  def head: A = this.header

  def tail: GenericList[A] = tailOb

  def add[B >: A](item: B): GenericList[B] = new ContentList(item, this)

  def isEmpty: Boolean = false

  def printElements: String = {
    if (tailOb.isEmpty) "" + header
    else header + "," + tailOb.printElements
  }

  def filter(predicate: MyPredicate[A]): GenericList[A] = {
    if (predicate.test(header)) new ContentList(header, tailOb.filter(predicate))
    else tailOb.filter(predicate)
  }

  def map[B](transformer: MyTransformer[A, B]): GenericList[B] = {
    new ContentList(transformer.transform(header), tailOb.map(transformer))
  }

  def ++[B >: A](list: GenericList[B]): GenericList[B] = {
    new ContentList(header, tailOb ++ list)
  }

  // for flatmap we need to create a concatenation method(++) in the abstract class.
  def flatMap[B](transformer: MyTransformer[A, GenericList[B]]): GenericList[B] = {
    transformer.transform(header) ++ tailOb.flatMap(transformer)
  }
}

trait MyPredicate[-T] {
  def test(elem: T): Boolean
}

trait MyTransformer[-A, B] {
  def transform(elem: A): B
}

object ListExtension extends App {
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
    listOfInts.flatMap((elem: Int) => new ContentList[Int](elem, new ContentList(elem + 1, MyEmptyList))).toString
  }")
}