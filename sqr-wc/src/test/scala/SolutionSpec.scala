package square

import org.specs2.mutable.Specification

class SolutionSpec extends Specification {

  "state is empty" >> {
    val expected = List(SquarePosition(1f,0f,2f))
    Solution.drop(1 ,2, List.empty) ==== expected
  }

  "2 boxes don't collide" >> {
    val actual = Solution.drop(3.1f ,2, List(SquarePosition(1f,0f,2f)))
    actual must haveSize(2)
    actual must contain(SquarePosition(1f,0f,2f),
      SquarePosition(3.1f,0f,2f))
  }

  "2 boxes exactly collide" >> {
    val currentState = List(SquarePosition(1f,0f,2f))
    val actual = Solution.drop(1f ,2f, currentState)

    actual must haveSize(2)
    actual must contain(
      SquarePosition(1f,0f,2f),
      SquarePosition(1f,2f,2f))
  }

  "2 boxes on top of misaligned" >> {
    val currentState = List(SquarePosition(1f,0f,2f))
    val actual = Solution.drop(1.5f ,2f, currentState)

    actual must haveSize(2)
    actual must contain(
      SquarePosition(1f,0f,2f),
      SquarePosition(1.5f,2f,2f))
  }

  "2 boxes on top of misaligned - 2" >> {
    val currentState = List(SquarePosition(1f,0f,2f))
    val actual = Solution.drop(0.5f ,2f, currentState)

    actual must haveSize(2)
    actual must contain(
      SquarePosition(1f,0f,2f),
      SquarePosition(0.5f,2f,2f))
  }

  "2 boxes on top of misaligned incoming box is too large" >> {
    val currentState = List(SquarePosition(1f,0f,2f))
    val actual = Solution.drop(0.5f ,4f, currentState)

    actual must haveSize(2)
    actual must contain(
      SquarePosition(1f,0f,2f),
      SquarePosition(0.5f,2f,4f))
  }
}
