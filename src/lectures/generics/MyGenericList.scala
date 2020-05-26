package lectures.generics

import lectures.generics.advanced.{ContentList, MyEmptyList}

/**
 * With the list implementation in the class:
 *  lectures.objectoriented.MyList, we were able to create a list of integers. The limitation with such an
 * implementation is that we can only use it for creating a list of integers, we cannot use the same class for creating
 * a list of Strings or float or etc. When such diversity is needed in programming we use the concept of generics. The
 * following class is a generic list implementation with which we can create any sort of list.
 *
 * This will also use the concept of the terms: (+A)Covariance, (A)Invariance and Contravariance(-A).
 * We will also see bounded context being used in such an implementation which is [B <:A] or [B >:A]
 */

// Defined an abstract list which is covariant in type A.
abstract class MyGenericList[+A] {
  def head: A

  def tail: MyGenericList[A]

  // This defines B as a supertype of A
  def add[B >: A](item: B): MyGenericList[B]

  def isEmpty: Boolean

  def printElements: String

  // Uses polymorphism to print the list elements recursively
  override def toString: String = "[" + printElements + "]"
}

// We will need to extend the abstract class by 2 components, 1 an object denoting empty list,
// and 2nd a class denoting non-empty list.

object EmptyList extends MyGenericList[Nothing] {
  def head: Nothing = throw new NoSuchElementException

  def tail: MyGenericList[Nothing] = throw new NoSuchElementException
  // This defines B as a supertype of Nothing which is any possible type in the scala type system.
  def add[B >: Nothing](item: B): MyGenericList[B] = new List(item, EmptyList)

  def isEmpty: Boolean = true

  // Since this is an empty list it doesn't have any elements.
  def printElements: String = ""
}

class List[+A](header: A, tailOb: MyGenericList[A]) extends MyGenericList[A] {
  def head: A = this.header

  def tail: MyGenericList[A] = tailOb

  def add[B >: A](item: B): MyGenericList[B] = new List(item, this)

  def isEmpty: Boolean = false

  def printElements: String = {
    if (tailOb.isEmpty) "" + header
    else header + "," + tailOb.printElements
  }
}

object ListPlayground extends App {
  val listOfInts = new ContentList(1, new ContentList(2, new ContentList(3, MyEmptyList)))
  val listOfStrings = new ContentList("hello", new ContentList("generics", new ContentList("in scala", MyEmptyList)))

  println(s"list of ints = $listOfInts \n and list of strings = $listOfStrings")

  /*And here is the o/p of the generic list implementation:
  list of ints = [1,2,3]
    and list of strings = [hello,generics,in scala]
*/
}