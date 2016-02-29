package square

import org.specs2.mutable.Specification

class SolutionSpec extends Specification {

  "no zero in the matrix" >> {
    val input0 = List(
      List(1, 1, 1)
    )
    Solution.largestArea(input0) ==== 3

    val input1 = List(
      List(1, 1),
      List(1, 1)
    )
    Solution.largestArea(input1) ==== 4

    val input2 = List(
      List(1, 1, 1),
      List(1, 1, 1),
      List(1, 1, 1)
    )
    Solution.largestArea(input2) ==== 9

    val input3 = List(
      List(1, 1, 1, 1),
      List(1, 1, 1, 1),
      List(1, 1, 1, 1),
      List(1, 1, 1, 1)
    )
    Solution.largestArea(input3) ==== 16

    val input4 = List(
      List(1, 1, 1, 1, 1),
      List(1, 1, 1, 1, 1)
    )
    Solution.largestArea(input4) ==== 10

    val input5 = List(
      List(1, 1, 1, 1),
      List(1, 1, 1, 1),
      List(1, 1, 1, 1)
    )
    Solution.largestArea(input5) ==== 12
  }

  "one zero on the first column" >> {
    val input1 = List(
      List(0, 1, 1, 1, 1),
      List(1, 1, 1, 1, 1)
    )
    Solution.largestArea(input1) ==== 8

    val input11 = List(
      List(0, 1),
      List(1, 1)
    )
    Solution.largestArea(input11) ==== 2

    val input12 = List(
      List(0, 1, 1),
      List(1, 1, 1)
    )
    Solution.largestArea(input12) ==== 4

    val input2 = List(
      List(0, 1, 1),
      List(1, 1, 1),
      List(1, 1, 1),
      List(1, 1, 1),
      List(1, 1, 1)
    )
    Solution.largestArea(input2) ==== 12

    val input3 = List(
      List(1, 1, 1),
      List(0, 1, 1),
      List(1, 1, 1),
      List(1, 1, 1),
      List(1, 1, 1)
    )
    Solution.largestArea(input3) ==== 10

    val input4 = List(
      List(1, 1),
      List(0, 1),
      List(1, 1),
      List(1, 1),
      List(1, 1)
    )
    Solution.largestArea(input4) ==== 6

    val input5 = List(
      List(1, 1, 1),
      List(1, 1, 1),
      List(1, 1, 1),
      List(1, 1, 1),
      List(1, 1, 1),
      List(0, 1, 1)
    )
    Solution.largestArea(input5) ==== 15

    val input6 = List(
      List(1, 1),
      List(1, 1),
      List(1, 1),
      List(1, 1),
      List(0, 1),
      List(1, 1),
      List(1, 1)
    )
    Solution.largestArea(input6) ==== 8
  }

  "multiple zeros on the first column" >> {
    val input1 = List(
      List(0, 1, 1, 1, 1),
      List(0, 1, 1, 1, 1)
    )
    Solution.largestArea(input1) ==== 8

    val input2 = List(
      List(1, 1),
      List(1, 1),
      List(1, 1),
      List(0, 1),
      List(0, 1)
    )
    Solution.largestArea(input2) ==== 6

    val input3 = List(
      List(1, 1),
      List(1, 1),
      List(1, 1),
      List(0, 1),
      List(1, 1),
      List(1, 1),
      List(0, 1)
    )
    Solution.largestArea(input3) ==== 7
  }

  "one zero on the first row" >> {
    val input1 = List(
      List(1, 0, 1, 1, 1),
      List(1, 1, 1, 1, 1)
    )
    Solution.largestArea(input1) ==== 6

    val input2 = List(
      List(1, 1, 1, 1, 0),
      List(1, 1, 1, 1, 1)
    )
    Solution.largestArea(input2) ==== 8

    val input3 = List(
      List(1, 1, 1, 0, 1, 1, 1, 1, 1),
      List(1, 1, 1, 1, 1, 1, 1, 1, 1)
    )
    Solution.largestArea(input3) ==== 10
  }

  "multiple zeros on the first row" >> {

    val input1 = List(
      List(1, 1, 1, 0, 1, 1, 0, 1, 1),
      List(1, 1, 1, 1, 1, 1, 1, 1, 1)
    )
    Solution.largestArea(input1) ==== 9
  }

  "one zero anywhere" >> {

    val input1 = List(
      List(1, 1, 1, 1, 1, 1, 1, 1, 1),
      List(1, 1, 1, 1, 1, 1, 1, 1, 1),
      List(1, 1, 1, 0, 1, 1, 1, 1, 1),
      List(1, 1, 1, 1, 1, 1, 1, 1, 1)
    )
    Solution.largestArea(input1) ==== 20

    val input2 = List(
      List(1, 1, 1, 1, 1, 1, 1, 1, 1),
      List(1, 1, 1, 1, 1, 1, 0, 1, 1),
      List(1, 1, 1, 1, 1, 1, 1, 1, 1),
      List(1, 1, 1, 1, 1, 1, 1, 1, 1)
    )
    Solution.largestArea(input2) ==== 24
  }

  "complete example" >> {

    val input1 = List(
      List(1, 1, 1, 1, 0, 1, 1, 1, 1),
      List(1, 1, 1, 1, 1, 1, 0, 1, 1),
      List(1, 1, 1, 0, 1, 1, 1, 1, 1),
      List(1, 1, 1, 1, 1, 1, 1, 1, 1)
    )
    Solution.largestArea(input1) ==== 12


    val input2 = List(
      List(1, 0),
      List(0, 1)
    )
    Solution.largestArea(input2) ==== 1
  }

}
































