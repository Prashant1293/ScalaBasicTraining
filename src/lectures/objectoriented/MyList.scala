package lectures.objectoriented

/**
 * The question here is to generate our own List collection with the following basic functionality:
 * 1. Head : Returns the first element of this List.
 * 2. Tail : Returns the remainder of the list.
 * 3. Add : Adds an integer value to this list.
 * 4. toString : Returns an overridden string implementation of this List.
 * 5. isEmpty : Returns a boolean depending on whether the list is empty.
 */

// Declares the behavior we want our list to have.
abstract class MyList {
  def head: Int

  def tail: MyList

  def add(item: Int): MyList

  def isEmpty: Boolean

  def printElements: String

  // Uses polymorphism to print the list elements recursively
  override def toString: String = "[" + printElements + "]"
}

// We will need to extend the abstract class by 2 components, 1 an object denoting empty list,
// and 2nd a class denoting non-empty list.

object EmptyList extends MyList {
  def head: Int = throw new NoSuchElementException

  def tail: MyList = throw new NoSuchElementException

  def add(item: Int): MyList = new List(item, EmptyList)

  def isEmpty: Boolean = true

  // Since this is an empty list it doesn't have any elements.
  def printElements: String = ""
}

class List(header: Int, tailOb: MyList) extends MyList {
  def head: Int = this.header

  def tail: MyList = tailOb

  def add(item: Int): MyList = new List(item, this)

  def isEmpty: Boolean = false

  def printElements: String = {
    if (tailOb.isEmpty) "" + header
    else header + "," + tailOb.printElements
  }

}

object MyListApp extends App {
  val list = new List(1, EmptyList).add(2).add(3)
  println("My list is = " + list)
  println("Is my list empty? " + list.isEmpty)
  println("Head is at = " + list.head)
  println("Tail of my list is = " + list.tail)
  val modifiedList = list.add(4) // Since list in scala are immutable :D always a new instance when a new value is added.
  println("Modified list = " + modifiedList)

  println("When toString() on the list is called, output is = " + list.toString)

  // Here is the output of the program.
  /*
  My list is = [3,2,1]
Is my list empty? false
Head is at = 3
Tail of my list is = [2,1]
Modified list = [4,3,2,1]
When toString() on the list is called, output is = [3,2,1]
   */
}