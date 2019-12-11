import scala.collection.mutable

trait Direction {
  def moveLeft: Direction
  def moveRight: Direction
}

object Up extends Direction {
  override def moveLeft: Direction = Left
  override def moveRight: Direction = Right
}

object Down extends Direction {
  override def moveLeft: Direction = Right
  override def moveRight: Direction = Left
}

object Left extends Direction {
  override def moveLeft: Direction = Down
  override def moveRight: Direction = Up
}

object Right extends Direction {
  override def moveLeft: Direction = Up
  override def moveRight: Direction = Down
}

case class Robot(program: String) {
  private val brain = IntcodeComputer(None, program, stopOnOutput = true)
  private var direction: Direction = Up
  private var position: Point = Point(0, 0)
  val paintedPanels: mutable.Map[Point, Int] = mutable.Map[Point, Int]()

  private def move(): Unit = {
    position = direction match {
      case Up => position.toUp
      case Down => position.toDown
      case Left => position.toLeft
      case Right => position.toRight
    }
  }

  @scala.annotation.tailrec
  final def paint(): Unit = {
    val input = paintedPanels.getOrElse(position, 0).toLong
    brain.setDefaultInput(input)
    val outputs = brain.executeProgram() ++ brain.executeProgram()
    if (outputs.nonEmpty) {
      paintedPanels(position) = outputs(0).toInt
      if (outputs.size > 1) {
        direction = outputs(1) match {
          case 0 => direction.moveLeft
          case _ => direction.moveRight
        }
        move()
      }
      paint()
    }
  }

  def drawImage(): Unit = {
    val maxX = paintedPanels.keys.maxBy(_.x).x.abs
    val minY = paintedPanels.keys.minBy(_.y).y.abs

    val image: Seq[Vector[Int]] = (0 to minY).map { y =>
      (0 to maxX).map(x => paintedPanels.getOrElse(Point(x, -y), 0)).toVector
    }

    image.foreach { line =>
      line.foreach {
        case 0 => print(" ")
        case 1 => print("X")
      }
      println()
    }
  }

  final def numberOfPanels(): Int = {
    paintedPanels.keys.toSet.size
  }

}
