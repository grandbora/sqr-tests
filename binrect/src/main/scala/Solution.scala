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

          // COLS

          val firstZeroX = matrix(firstZeroY).indexOf(0)
          val dropBeforeColsMatrix = matrix.map(_.drop(firstZeroX + 1))
          val dropBeforeColsMatrixArea = largestArea(dropBeforeColsMatrix)

          val dropAfterColsMatrix = matrix.map(row => row.dropRight(row.length - firstZeroX))
          val dropAfterColsMatrixArea = largestArea(dropAfterColsMatrix)

          // ROWS

          val dropBeforeRowsMatrix = matrix.drop(firstZeroY + 1)
          val dropBeforeRowsMatrixArea = largestArea(dropBeforeRowsMatrix)

          val dropAfterRowsMatrix = matrix.dropRight(matrix.length - firstZeroY)
          val dropAfterRowsMatrixArea = largestArea(dropAfterRowsMatrix)

          Math.max(
            Math.max(dropBeforeColsMatrixArea, dropAfterColsMatrixArea),
            Math.max(dropBeforeRowsMatrixArea, dropAfterRowsMatrixArea)
          )
        }

    }
  }

}










