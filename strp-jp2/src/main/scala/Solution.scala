package square

import scala.collection.mutable

// You're running a pool of servers where the servers are numbered
// sequentially starting from 1. Over time, any given server might explode,
// in which case its server number is made available for reuse.
// When a new server is launched, it should be given the lowest available number.
//
// Write a function which, given the list of currently allocated server numbers,
// returns the number of the next server to allocate.
// In addition, you should demonstrate your approach to testing that your
// function is correct.
// You may choose to use an existing testing library for your language
// if you choose, or you may write your own process if you prefer.
//
// For example, your function should behave something like the following:
//
//>> next_server_number([5, 3, 1])
//2
//>> next_server_number([5, 4, 1, 2])
//3
//>> next_server_number([3, 2, 1])
//4
//>> next_server_number([2, 3])
//1

// Server names consist of an alphabetic host type (e.g. "apibox")
// concatenated with the server number, with server numbers allocated as before
// (so "apibox1", "apibox2", etc. are valid hostnames).
//
//Write a name tracking class with two operations,
// allocate(host_type) and deallocate(hostname). The former should reserve and
// return the next available hostname,
// while the latter should release that hostname back into the pool.
//
//For example:
//
//>> tracker = Tracker.new()
//>> tracker.allocate("apibox")
//"apibox1"
//>> tracker.allocate("apibox")
//"apibox2"
//>> tracker.deallocate("apibox1")
//nil
//>> tracker.allocate("apibox")
//"apibox1"
//>> tracker.allocate("sitebox")
//"sitebox1"

class Tracker {

  val state = mutable.Map.empty[String, List[Int]]

  def allocate(hostType: String): String = {
    val servers = state.getOrElse(hostType, Nil)
    val lastAdded = Solution.next_server_number(servers)

    state.put(hostType, servers :+ lastAdded)

    s"${hostType}${lastAdded}"
  }

  def deallocate(server: String): Boolean = {
    val serverIdString = server.toCharArray.filter(_.isDigit).mkString("")
    val serverId = serverIdString.toInt // may fail ??
    val hostType = server.toCharArray.dropRight(serverIdString.length).mkString("")

    state.get(hostType) match {
      case Some(currentServers) if currentServers.contains(serverId) =>
        state.put(hostType, currentServers diff List(serverId))
        true
      case _ =>
        false
    }
  }
}

object Solution {

  def next_server_number(servers: List[Int]): Int = {

    val orderedServers = servers.toSet.toList.sorted

    findTheGap(orderedServers) match {
      case Some((_, missing)) => missing
      case None => orderedServers.length + 1
    }
  }

  private def findTheGap(servers: List[Int]): Option[(Int, Int)] = {
    val ordered = (1 to servers.length).toList
    servers.zip(ordered).find { case (serverId, orderedId) => serverId != orderedId }
  }

}
