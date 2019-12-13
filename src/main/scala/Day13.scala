
object PixelType extends Enumeration {

  type PixelType = Value
  val Empty: PixelType.Value = Value
  val Wall: PixelType.Value = Value
  val Block: PixelType.Value = Value
  val Paddle: PixelType.Value = Value
  val Ball: PixelType.Value = Value
}

case class Day13(program: String) {

  private val game = IntcodeComputer(None, program, stopOnOutput = true)

  def play(): Map[(Long, Long), Long] = {

    @scala.annotation.tailrec
    def iterate(previousBoard: Map[(Long, Long), Long]): Map[(Long, Long), Long] = {

      def calculateInput(): Long = {
        def findPixelX(pixelToFind: PixelType.Value): Long = {
          previousBoard.find { case (_, pixel) =>
            pixel == pixelToFind.id
          }.map { case ((x, _), _) =>
            x
          }.getOrElse(0)
        }

        val ballX: Long = findPixelX(PixelType.Ball)
        val paddleX: Long = findPixelX(PixelType.Paddle)

        if (ballX < paddleX) -1
        else if (ballX > paddleX) 1
        else 0
      }

      game.setInputCallback(calculateInput)
      val newOutput = game.executeProgram() ++ game.executeProgram() ++ game.executeProgram()
      if (newOutput.isEmpty)
        previousBoard
      else {
        val newBoard = previousBoard + ((newOutput.head, newOutput(1)) -> newOutput(2))
        iterate(newBoard)
      }
    }

    iterate(Map[(Long, Long), Long]())
  }

  def numberOfBlocksAfterPlaying(): Long = {
    play().values.count(_ == PixelType.Block.id)
  }
}

object Day13 {
  def calculateScore(board: Map[(Long, Long), Long]): Long = board.get(-1, 0).getOrElse(-1)
  def numberOfBlocks(board: Map[(Long, Long), Long]): Long = board.values.count(_ == PixelType.Block.id)
}