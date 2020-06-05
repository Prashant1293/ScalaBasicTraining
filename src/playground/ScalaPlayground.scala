package playground

object ScalaPlayground extends App {
  def getInt(throwException: Boolean): Int = {
    if (throwException) throw new RuntimeException("Exception was thrown forcefully.")
    26
  }

  val exception:AnyVal = try {
    getInt(false)
  } catch {
    case e: NullPointerException => println("Nothing will be printed.")
    case e: RuntimeException => println(s"Exception caught with message = ${e.getMessage}")
  } finally {
    println("Anything in the finally block will always be executed.")
  }

  println(exception)

}
