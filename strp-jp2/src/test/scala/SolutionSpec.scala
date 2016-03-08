package square

import org.specs2.mutable.Specification


class TrackerSpec extends Specification {

  "tracker is empty" >> {
    val tracker = new Tracker

    tracker.allocate("apibox") ==== "apibox1"
    tracker.deallocate("apibox1") ==== true

    tracker.allocate("sitebox") ==== "sitebox1"
    tracker.deallocate("sitebox1") ==== true
  }

  "tracker has one server in it" >> {
    val tracker = new Tracker

    tracker.allocate("apibox")
    tracker.allocate("apibox") ==== "apibox2"
    tracker.deallocate("apibox2") ==== true

    tracker.allocate("apibox") ==== "apibox2"
  }

  "tracker doesn't remove nonexisting servers" >> {
    val tracker = new Tracker

    tracker.allocate("apibox")
    tracker.deallocate("apibox2") ==== false
    tracker.deallocate("sitebox1") ==== false
  }

  "multiple servers added to the tracker without removing any" >> {
    val tracker = new Tracker

    tracker.allocate("apibox")
    tracker.allocate("apibox") ==== "apibox2"
    tracker.allocate("apibox") ==== "apibox3"

    tracker.allocate("sitebox") ==== "sitebox1"
    tracker.allocate("sitebox") ==== "sitebox2"

    tracker.allocate("apibox") ==== "apibox4"
    tracker.allocate("apibox") ==== "apibox5"

    tracker.allocate("sitebox") ==== "sitebox3"
    tracker.allocate("sitebox") ==== "sitebox4"
  }

  "returns removed id when a server is removed" >> {
    val tracker = new Tracker

    tracker.allocate("apibox")
    tracker.allocate("apibox")
    tracker.allocate("apibox")
    tracker.deallocate("apibox2") ==== true
    tracker.allocate("apibox") ==== "apibox2"
    tracker.allocate("apibox") ==== "apibox4"
    tracker.deallocate("apibox1") ==== true
    tracker.allocate("apibox") ==== "apibox1"
  }

  "handles multiple add/remove and hosttypes" >> {
    val tracker = new Tracker

    tracker.allocate("apibox")
    tracker.allocate("sitebox")
    tracker.allocate("apibox")
    tracker.allocate("sitebox")
    tracker.allocate("apibox")

    tracker.deallocate("apibox2") ==== true
    tracker.deallocate("sitebox1") ==== true

    tracker.allocate("sitebox") ==== "sitebox1"
    tracker.allocate("apibox") ==== "apibox2"

    tracker.deallocate("apibox1") ==== true
    tracker.allocate("apibox") ==== "apibox1"

    tracker.allocate("sitebox") ==== "sitebox3"
    tracker.allocate("sitebox") ==== "sitebox4"
  }

}


class SolutionSpec extends Specification {

  "returns 1 when there are no servers" >> {
    Solution.next_server_number(List.empty) ==== 1
  }

  "returns 2 when there is only server 1" >> {
    Solution.next_server_number(List(1)) ==== 2
  }

  "returns next number when there are no gaps" >> {
    Solution.next_server_number(List(1, 2)) ==== 3
    Solution.next_server_number(List(1, 2, 3)) ==== 4
    Solution.next_server_number(List(1, 2, 3, 4)) ==== 5
  }

  "returns missing number when there is a gap" >> {
    Solution.next_server_number(List(2)) ==== 1
    Solution.next_server_number(List(1, 2, 4)) ==== 3
    Solution.next_server_number(List(1, 3, 4)) ==== 2
    Solution.next_server_number(List(2, 3, 4)) ==== 1
    Solution.next_server_number(List(1, 2, 4, 5)) ==== 3
  }

  "returns lowest missing number when there are multiple gaps" >> {
    Solution.next_server_number(List(2, 4)) ==== 1
    Solution.next_server_number(List(1, 4)) ==== 2
    Solution.next_server_number(List(1, 2, 4, 7)) ==== 3
    Solution.next_server_number(List(1, 4, 7)) ==== 2
    Solution.next_server_number(List(1, 2, 3, 7)) ==== 4
  }

  "returns lowest next number when input is out of order" >> {
    Solution.next_server_number(List(2, 1)) ==== 3
    Solution.next_server_number(List(2, 3, 1)) ==== 4
    Solution.next_server_number(List(4, 3, 1, 2)) ==== 5
    Solution.next_server_number(List(2, 4, 1)) ==== 3
    Solution.next_server_number(List(4, 2, 3)) ==== 1
    Solution.next_server_number(List(1, 7, 2, 3)) ==== 4
  }

  "returns lowest next number when input has duplicates" >> {
    Solution.next_server_number(List(2, 1, 2, 2)) ==== 3
    Solution.next_server_number(List(2, 1, 1, 1, 2, 2)) ==== 3
    Solution.next_server_number(List(2, 3, 1, 3, 2, 1, 1)) ==== 4
    Solution.next_server_number(List(4, 3, 1, 2, 4, 2, 1)) ==== 5
    Solution.next_server_number(List(1, 7, 2, 3, 2, 3)) ==== 4
  }
}
