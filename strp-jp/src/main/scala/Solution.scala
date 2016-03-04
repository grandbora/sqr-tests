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

    val (newServers, lastAdded) = state.get("apibox") match {
      case None => List(1) -> 1
      case Some(servers) => (servers :+ servers.length + 1) -> (servers.length + 1)
    }

    state.put(hostType, newServers)
    s"$hostType$lastAdded"
  }

  def deallocate(hostName: String): Boolean = {
    val serverNumberStr = hostName.toCharArray.toList.filter(_.isDigit).mkString("")
    val serverNumber = serverNumberStr.toInt
    val hostType = hostName.toCharArray.dropRight(serverNumberStr.length).mkString("")

    val newServers = state.get(hostType) match {
      case None => ???
      case Some(servers) => servers diff List(serverNumber)
    }

    state.put(hostType, newServers)

    true
  }

  def showState = {
    state
  }
}


object Solution extends App {

  def next_server_number(serverNumberList: List[Int]): Int = {

    val sortedList = serverNumberList.toSet.toList.sorted

    if (isThereGap(sortedList)) {
      val firstGap = sortedList.zip((1 to sortedList.length).toList).find {
        case (serverNumber, sequentialNumber) =>
          serverNumber != sequentialNumber
      }

      firstGap match {
        case None => throw new Exception("Unexpected case")
        case Some((_, sequentialNumber)) =>
          sequentialNumber
      }

    } else {
      sortedList.length + 1
    }


  }

  private def isThereGap(serverNumber: List[Int]): Boolean = {
    (1 to serverNumber.length).toList != serverNumber
  }

}

