package lectures.functions

class FunctionalList {

}

/**
 * Well in this particular list implementation we are going to update the predicate and transformer code to leverage the
 * Function[] capability of Scala. This kind of implementation moves our code away from Object oriented programming and
 * leads towards functional programming. The most benefited methods would be map, flatMap, and filter here.
 *
 * Now let's enhance this list with the implementations for the forEach, sort, zipWith and fold methods using HOF and
 * currying.
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

  // HOF's start from here.
  def forEach(f: A => Unit): Unit

  def sort(compare: (A, A) => Int): GenericListFunctional[A]

  def zipWith[B, C](list: GenericListFunctional[B], zip: (A, B) => C): GenericListFunctional[C]

  // This is a partial function definition for fold use currying instead.
  // def fold[B](start: B, folder: (A, B) => B): B

  def fold[B](start: B)(operator: (B, A) => B): B
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

  // An empty list doesn't need to print anything to the console since it is empty.
  override def forEach(f: Nothing => Unit): Unit = ()

  override def sort(compare: (Nothing, Nothing) => Int): GenericListFunctional[Nothing] = MyEmptyList

  override def zipWith[B, C](list: GenericListFunctional[B], zip: (Nothing, B) => C): GenericListFunctional[C] = {
    if (!list.isEmpty) throw new RuntimeException("Lists length differs.")
    else MyEmptyList
  }

  // This is a partial function definition for fold use currying instead.
  // override def fold[B](start: B, folder: (Nothing, B) => B): B = start
  override def fold[B](start: B)(operator: (B, Nothing) => B): B = start
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

  // A for each should print each element in the list till the end is reached.
  override def forEach(f: A => Unit): Unit = {
    f(header)
    tailOb.forEach(f)
  }

  // A sort should be able to sort this list on the basis of sort function received.
  override def sort(compare: (A, A) => Int): GenericListFunctional[A] = {
    def insert(headValue: A, sortedList: GenericListFunctional[A]): ContentList[A] = {
      if (sortedList.isEmpty) ContentList(headValue, MyEmptyList)
      else if (compare(headValue, sortedList.head) <= 0) ContentList(headValue, sortedList)
      else ContentList(sortedList.head, insert(headValue, sortedList.tail))
    }

    val sortedTail = tailOb.sort(compare)
    insert(head, sortedTail)
  }

  override def zipWith[B, C](list: GenericListFunctional[B], zip: (A, B) => C): GenericListFunctional[C] = {
    if (list.isEmpty) throw new RuntimeException("Lists length differs.")
    else new ContentList[C](zip(head, list.head), tailOb.zipWith(list.tail, zip))
  }

  // This is a partial function definition for fold use currying instead.
  /*override def fold[B](start: B, folder: (A, B) => B): B = {
    if (tailOb.isEmpty)
      folder(head, start)
    else
      tailOb.fold(folder(head, start), folder)
  }*/

  override def fold[B](start: B)(operator: (B, A) => B): B = {
    tailOb.fold(operator(start, head))(operator)
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
}

object FunctionalListExtension extends App {
  val listOfInts = new ContentList(1, new ContentList(2, new ContentList(3, MyEmptyList))).add(4).add(5)
  val listOfStrings = new ContentList("hello", new ContentList("generics", new ContentList("in scala", MyEmptyList)))
  val anotherListOfStrings = new ContentList("hello", new ContentList("generics", new ContentList("in scala", MyEmptyList))).add("Sample").add("zipWith")

  println(s"list of ints = $listOfInts \nand list of strings = $listOfStrings")

  /*And here is the o/p of the generic list implementation:
  list of ints = [1,2,3]
    and list of strings = [hello,generics,in scala]
*/

  // Let us try apply the filter, map, and flatMap methods to our list.

  println(s"Filtered list with even numbers = ${
    listOfInts.filter(elem => elem % 2 == 0).toString
  }")

  println(s"Applied map to get the square of elements in the list = ${
    listOfInts.map(elem => elem * elem).toString
  }")

  println(s"Applying flatmap on the list = ${
    listOfInts.flatMap(new Function1[Int, GenericListFunctional[Int]] {
      override def apply(elem: Int): GenericListFunctional[Int] = new ContentList(elem, new ContentList(elem + 1, MyEmptyList))
    }).toString
  }")

  // Let's see how making case class and case object helps in our case. Also, now we do not need to use new keyword to
  // create instance of class. Case classes have their own apply method that takes care of object creation.
  val anotherListOfInts = ContentList(1, new ContentList(2, new ContentList(3, MyEmptyList))).add(4).add(5)

  println(listOfInts == anotherListOfInts)

  // HOF method calls.
  anotherListOfInts.forEach(x => println(x)) // Or we can simply write it as
  anotherListOfInts.forEach(println)

  println(anotherListOfInts.sort((x, y) => {
    println(s"x = $x, y = $y")
    y - x
  }))

  println(anotherListOfInts.zipWith(listOfInts, (x, y: Int) => x * y))
  println(anotherListOfStrings.zipWith[Int, String](listOfInts, (x, y) => x + "_" + y))
  println(anotherListOfStrings.zipWith[Int, String](listOfInts, (x, y) => x * y))
  /* This prints:
[25,16,1,4,9]
[zipWith_5,Sample_4,hello_1,generics_2,in scala_3]
[zipWithzipWithzipWithzipWithzipWith,SampleSampleSampleSample,hello,genericsgenerics,in scalain scalain scala]
*/

  println(anotherListOfInts.fold(1)((x, y) => x + y))
  println(anotherListOfInts.fold(2)(_ + _))
}
