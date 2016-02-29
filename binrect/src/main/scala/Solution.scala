package square

object Solution extends App {

  def largestArea(matrix: List[List[Int]]): Int = {

    matrix match {
      case Nil => 0

      case _ =>
        val firstZeroY = matrix.indexWhere(_.contains(0))

        if (firstZeroY == -1)
          matrix.head.length * matrix.length
        else {

          val columnWiseLargestAreaValue = columnWiseLargestArea(matrix,firstZeroY)
          val rowWiseLargestAreaValue = rowWiseLargestAre(matrix, firstZeroY)

          Math.max(
            columnWiseLargestAreaValue,
            rowWiseLargestAreaValue
          )
        }
    }
  }

  private def columnWiseLargestArea(matrix: List[List[Int]], firstZeroY:Int): Int = {
    val firstZeroX = matrix(firstZeroY).indexOf(0)

    val dropBeforeColsMatrix = matrix.map(_.drop(firstZeroX + 1))
    val dropBeforeColsMatrixArea = largestArea(dropBeforeColsMatrix)

    val dropAfterColsMatrix = matrix.map(row => row.dropRight(row.length - firstZeroX))
    val dropAfterColsMatrixArea = largestArea(dropAfterColsMatrix)

    Math.max(dropBeforeColsMatrixArea, dropAfterColsMatrixArea)
  }

  private def rowWiseLargestAre(matrix: List[List[Int]], firstZeroY: Int): Int = {
    val dropBeforeRowsMatrix = matrix.drop(firstZeroY + 1)
    val dropBeforeRowsMatrixArea = largestArea(dropBeforeRowsMatrix)

    val dropAfterRowsMatrix = matrix.dropRight(matrix.length - firstZeroY)
    val dropAfterRowsMatrixArea = largestArea(dropAfterRowsMatrix)
    Math.max(dropBeforeRowsMatrixArea, dropAfterRowsMatrixArea)
  }

}








