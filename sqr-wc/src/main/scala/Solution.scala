package square

case class SquarePosition(x: Float, y: Float, dimension: Float)

object Solution extends App {

  def drop(x: Float, dimension: Float, currentState: List[SquarePosition]): List[SquarePosition] = {

    val occupyingSqs = currentState.filter {
      sqPos =>
        (
          (x >= sqPos.x && x < sqPos.x + sqPos.dimension) ||
            (x + dimension >= sqPos.x && x + dimension < sqPos.x + sqPos.dimension)
          ) ||
          (
            (sqPos.x >= x && sqPos.x < x + dimension) ||
              (sqPos.x + sqPos.dimension >= x && sqPos.x + sqPos.dimension < x + dimension)
            )

    }

    occupyingSqs match {
      case Nil =>
        val newIncomingPos = SquarePosition(x, 0, dimension)
        newIncomingPos :: currentState
      case _ =>
        val sorted = occupyingSqs.sortWith {
          case (sq1, sq2) =>
            (sq1.y + sq1.dimension) > (sq2.y + sq2.dimension)
        }
        val newYPos = sorted.head.y + sorted.head.dimension
        val newIncomingPos = SquarePosition(x, newYPos, dimension)
        newIncomingPos :: currentState
    }
  }
}
