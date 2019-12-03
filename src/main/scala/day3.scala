
case class Point(x: Int, y: Int) {
  def toLeft = Point(x-1, y)
  def toRight = Point(x+1, y)
  def toUp = Point(x, y+1)
  def toDown = Point(x, y-1)

  def calculateManhattanDistanceToCenter: Int = {
    x.abs + y.abs
  }
}

object day3 {
  def findIntersectionClosestToTheCenter(wire1: String, wire2: String): Int = {
    getPoints(wire1).tail.intersect(getPoints(wire2).tail).map(_.calculateManhattanDistanceToCenter).min
  }

  def move(points: Vector[Point], toFunc: Point => Point): Vector[Point] = {
    points :+ toFunc(points.last)
  }

  def getPoints(wire: String): Vector[Point] = {
    wire.split(",").foldLeft(Vector(Point(0,0))) { (previousPoints, newInstruction) =>
      val numberOfMovements = newInstruction.tail.toInt

      val toFunc: Point => Point = newInstruction.head match {
        case 'L' => _.toLeft
        case 'R' => _.toRight
        case 'U' => _.toUp
        case 'D' => _.toDown
      }

      (1 to numberOfMovements).foldLeft(previousPoints) { (prev, _) =>
        move(prev, toFunc)
      }
    }
  }

  def findShorterIntersection(wire1: String, wire2: String): Int = {
    val pointsForWire1 = getPoints(wire1)
    val pointsForWire2 = getPoints(wire2)
    val intersections = pointsForWire1.intersect(pointsForWire2)
    intersections.tail.map { intersection =>
      pointsForWire1.indexOf(intersection) + pointsForWire2.indexOf(intersection)
    }.min
  }

}