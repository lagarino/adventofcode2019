
object day2 {

  def programAlarm(numbers: String, input1: Int, input2: Int): Int = {
    val memory = numbers.split(",").map(_.toInt)
    memory(1) = input1
    memory(2) = input2
    executeInstruction(memory, 0)(0)
  }

  def findInputsForOutput(numbers: String, output: Int): (Int, Int) = {
    val potentialInput1 = 0 to 99
    val potentialInput2 = 0 to 99
    val validInputPairs = for  {
      x <- potentialInput1
      y <- potentialInput2
      calculatedOutput = programAlarm(numbers, x, y)
      if calculatedOutput == output
    } yield (x,y)
    validInputPairs(0)
  }

  @scala.annotation.tailrec
  private def executeInstruction(memory: Array[Int], instructionPointer: Int): Array[Int] = {
    val operation = memory(instructionPointer)
    operation match {
      case 99 =>
        memory
      case 1 =>
        val newValue = memory(memory(instructionPointer+1)) + memory(memory(instructionPointer+2))
        memory(memory(instructionPointer+3)) = newValue
        executeInstruction(memory, instructionPointer+4)
      case 2 =>
        val newValue = memory(memory(instructionPointer+1)) * memory(memory(instructionPointer+2))
        memory(memory(instructionPointer+3)) = newValue
        executeInstruction(memory, instructionPointer+4)
    }
  }

}