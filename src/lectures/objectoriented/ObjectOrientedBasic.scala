package lectures.objectoriented

object ObjectOrientedBasic extends App {
  val author: Writer = new Writer("Prashant", "Sharma", 1993)
  val novel: Novel = new Novel("MonkeyKong", 2020, author)

  println(s"Author = ${author.fullName()}")
  println(s"Age = ${novel.authorAge()}")
  println(novel.isWrittenBy("SharmaJi"))
  println(novel.isWrittenBy(author))
  println(novel.copy(2019))

  val counter: Counter = new Counter(0)
  println(counter.increment().currentCount())
  println(counter.increment(10).currentCount())
}

class Writer(val firstName: String, val surName: String, val year: Int) {
  def fullName(): String = {
    firstName + " " + surName
  }
}

class Novel(val name: String, val yearOfRelease: Int, val author: Writer) {
  def authorAge(): Int = {
    yearOfRelease - author.year
  }

  def isWrittenBy(author: String): Boolean = {
    if (this.author.fullName().equals(author)) {
      true
    } else false
  }

  def isWrittenBy(author: Writer): Boolean = author == this.author

  def copy(year: Int): Novel = new Novel(name, year, author)

  override def toString: _root_.java.lang.String = "Author: " + this.author.fullName() + ", novel: " + this.name + ", release: " + yearOfRelease
}

class Counter(val count: Int) {
  def currentCount(): Int = {
    count
  }

  def increment(): Counter = {
    new Counter(this.count + 1)
  }

  def increment(value: Int): Counter = {
    new Counter(count + value)
  }
}