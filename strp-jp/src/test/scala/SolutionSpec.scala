package square

import org.specs2.mutable.Specification

class TrackerSpec extends Specification {
  "state is empty" >> {
    val tracker = new Tracker

    tracker.state must beEmpty

    tracker.allocate("apibox") ==== "apibox1"

    tracker.state must haveSize(1)
    tracker.state must haveKey("apibox")

    tracker.deallocate("apibox1") ==== true
    tracker.allocate("apibox") ==== "apibox1"
  }

  "state has one inst of same hosttype" >> {
    val tracker = new Tracker
    tracker.allocate("apibox") ==== "apibox1"

    tracker.allocate("apibox") ==== "apibox2"
    tracker.deallocate("apibox2") ==== true
    tracker.allocate("apibox") ==== "apibox2"

  }
}

class SolutionSpec extends Specification {

  "empty server list is given" >> {
    Solution.next_server_number(List.empty) ==== 1
  }

  "server list has only `1` in it" >> {
    Solution.next_server_number(List(1)) ==== 2
  }

  "server list has sequentil server numbers in it" >> {
    Solution.next_server_number(List(1, 2)) ==== 3
    Solution.next_server_number(List(1, 2, 3, 4)) ==== 5
    Solution.next_server_number(List(1, 2, 3, 4, 5)) ==== 6
  }

  "there is a gap in the beginning" >> {
    Solution.next_server_number(List(2)) ==== 1
  }

  "there is a gap in the beginning" >> {
    Solution.next_server_number(List(1, 3)) ==== 2
  }

  "there is a gap, list is not ordered" >> {
    Solution.next_server_number(List(4, 2, 1)) ==== 3
  }

  "there are multiple gaps" >> {
    Solution.next_server_number(List(1, 2, 4, 6)) ==== 3
    Solution.next_server_number(List(6, 1, 4, 2)) ==== 3
  }


  "examples" >> {
    Solution.next_server_number(List(5, 3, 1)) ==== 2
    Solution.next_server_number(List(5, 4, 1, 2)) ==== 3
    Solution.next_server_number(List(3, 2, 1)) ==== 4
    Solution.next_server_number(List(2, 3)) ==== 1
  }

  "there are duplicates" >> {
    Solution.next_server_number(List(1, 2, 2)) ==== 3
    Solution.next_server_number(List(1, 1, 2, 3)) ==== 4
  }


}
