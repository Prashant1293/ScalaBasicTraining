package lectures.mapsandtuples

import scala.annotation.tailrec

object MapPlayground extends App {
  val tupleFirst = "Jim" -> 555
  val tupleSecond = "JIM" -> 900
  val aMap = Map(tupleFirst, tupleSecond).withDefaultValue(-1)

  val anotherMap = aMap.map(pair => pair._1.toLowerCase -> pair._2)

  println(s"The initial map was: $aMap and after applying the map operation to generate new keys the new map is:"
    + s" $anotherMap.\nThis tells us that if same key is appended in a map the prev value for that key is overridden by "
    + s"the new value provided.")

  /*val person1 = "person1" -> List("person2", "person3")
  val person2 = "person2" -> List("person1", "person3")
  val person3 = "person3" -> List("person1", "person2", "person4")
  val person4 = "person4" -> List("person3")

  val network = Map(person1, person2, person3, person4).withDefaultValue(List.empty)
  println(network)
*/

  println("Our network is going to be a Map of String -> Set depicting the friends of people.")

  // Adds a new member to our social network platform.
  def add(network: Map[String, Set[String]], person: String): Map[String, Set[String]] = {
    network + (person -> Set())
  }

  // Adds a new friend to a person's existing friend list in the social network platform.
  def addFriend(network: Map[String, Set[String]], person: String, friend: String): Map[String, Set[String]] = {
    val friendsOfPerson = network(person)
    val friendsOfFriendToAdd = network(friend)
    network + (person -> (friendsOfPerson + friend)) + (friend -> (friendsOfFriendToAdd + person))
  }

  // Removes a friend from a person's existing friend list of the social network platform.
  def unfriend(network: Map[String, Set[String]], person: String, friendToRemove: String): Map[String, Set[String]] = {
    val friendOfPerson = network(person)
    val friendOfFriend = network(friendToRemove)

    network + (person -> (friendOfPerson - friendToRemove)) + (friendToRemove -> (friendOfFriend - person))
  }

  // Removes a person from the social network platform, and from the list of it's friends.
  def remove(network: Map[String, Set[String]], personToRemove: String): Map[String, Set[String]] = {

    @tailrec
    def remover(network: Map[String, Set[String]], friends: Set[String]): Map[String, Set[String]] = {
      if (friends isEmpty) network
      else remover(unfriend(network, friends.head, personToRemove), friends.tail)
    }

    val friendsRemoved = remover(network, network(personToRemove))

    friendsRemoved - personToRemove
  }

  // Returns the number of friends of a person received.
  def nFriends(network: Map[String, Set[String]], person: String): Int = {
    if (network(person).isEmpty) 0
    else network(person).size
  }

  // Returns the person with max number of friends and their count.
  def maxFriends(network: Map[String, Set[String]]): (String, Int) = {
    val person = network.maxBy(pair => pair._2.size)._1
    person -> network(person).size
  }

  // Returns the first person with no friends.
  @tailrec
  def noFriends(network: Map[String, Set[String]]): String = {
    if (network.head._2.isEmpty) network.head._1
    else if (network.tail.isEmpty) "No person without a friend."
    else noFriends(network.tail)
  }

  val persons = add(add(add(Map(), "ps"), "nb"), "js")
  val ps_nb = addFriend(persons, "ps", "nb")
  val ps_js = addFriend(ps_nb, "ps", "js")
  println(persons, ps_js)
  println(unfriend(ps_js, "ps", "js"))
  println(remove(ps_js, "nb"))
  println(s"friends of ps in ps_js are = ${nFriends(ps_js, "ps")}")
  println(maxFriends(ps_js))
  println(noFriends(ps_js))
  println(noFriends(unfriend(ps_js, "ps", "js")))
}

