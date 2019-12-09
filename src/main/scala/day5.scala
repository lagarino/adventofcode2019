import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class MemoryContents(contents: String) {
  val operation: Int = contents.takeRight(2).toInt
  def getPositionMode(order: Int): Int =
    if (contents.length >= order + 2) contents.takeRight(order + 2).head.toString.toInt else 0
}

case class Memory(contents: Array[Int]) {
  def getContent(pointer: Int): MemoryContents = {
    MemoryContents(contents(pointer).toString)
  }

  def setValue(pointer: Int, value: Int): Unit = {
    contents(contents(pointer)) = value
  }

  def getValueForParameter(pointer: Int, positionMode: Int = 0): Int = {
    if (positionMode == 0) contents(contents(pointer))
    else contents(pointer)
  }
}

case class IntcodeComputer(phase: Int, memoryContents: String, stopOnOutput: Boolean = false) {
  private val memory = memoryContents.split(",").map(_.toInt)
  private val inputsQueue = mutable.Queue[Int]()
  inputsQueue.enqueue(phase)
  private var outputs = ListBuffer[Int]()
  private var instructionPointerState: Int = 0

  def executeProgram(inputs: Int*): Vector[Int] = {
    outputs = ListBuffer[Int]()
    inputsQueue.enqueueAll(inputs)
    executeInstruction(Memory(memory), instructionPointerState)
    outputs.toVector
  }

  @scala.annotation.tailrec
  private def executeInstruction(memory: Memory, instructionPointer: Int): Memory = {
    val memoryContents = memory.getContent(instructionPointer)
    memoryContents.operation match {
      case 99 =>
        memory
      case 1 =>
        sumOp(memory, instructionPointer, memoryContents)
        executeInstruction(memory, instructionPointer+4)
      case 2 =>
        timesOp(memory, instructionPointer, memoryContents)
        executeInstruction(memory, instructionPointer+4)
      case 3 =>
        inputOp(memory, instructionPointer)
        executeInstruction(memory, instructionPointer+2)
      case 4 =>
        outputOp(memory, instructionPointer, memoryContents)
        if (!stopOnOutput)
          executeInstruction(memory, instructionPointer+2)
        else {
          instructionPointerState = instructionPointer+2
          memory
        }
      case 5 =>
        jumpIfTrueOp(memory, instructionPointer, memoryContents)
      case 6 =>
        jumpIfFalseOp(memory, instructionPointer, memoryContents)
      case 7 =>
        lessThanOp(memory, instructionPointer, memoryContents)
        executeInstruction(memory, instructionPointer+4)
      case 8 =>
        equalsOp(memory, instructionPointer, memoryContents)
        executeInstruction(memory, instructionPointer+4)
    }
  }

  private def outputOp(memory: Memory, instructionPointer: Int, memoryContents: MemoryContents) = {
    val positionMode = memoryContents.getPositionMode(1)
    val outputValue = memory.getValueForParameter(instructionPointer + 1, positionMode)
    outputs.addOne(outputValue)
  }

  private def inputOp(memory: Memory, instructionPointer: Int) = {
    val inputValue = inputsQueue.dequeue()
    memory.setValue(instructionPointer + 1, inputValue)
  }

  private def jumpIfFalseOp(memory: Memory, instructionPointer: Int, memoryContents: MemoryContents) = {
    val positionMode1 = memoryContents.getPositionMode(1)
    val positionMode2 = memoryContents.getPositionMode(2)
    if (memory.getValueForParameter(instructionPointer + 1, positionMode1) == 0) {
      val newInstructionPointer = memory.getValueForParameter(instructionPointer + 2, positionMode2)
      executeInstruction(memory, newInstructionPointer)
    } else {
      executeInstruction(memory, instructionPointer + 3)
    }
  }

  private def jumpIfTrueOp(memory: Memory, instructionPointer: Int, memoryContents: MemoryContents) = {
    val positionMode1 = memoryContents.getPositionMode(1)
    val positionMode2 = memoryContents.getPositionMode(2)
    if (memory.getValueForParameter(instructionPointer + 1, positionMode1) != 0) {
      val newInstructionPointer = memory.getValueForParameter(instructionPointer + 2, positionMode2)
      executeInstruction(memory, newInstructionPointer)
    } else {
      executeInstruction(memory, instructionPointer + 3)
    }
  }

  private def timesOp(memory: Memory, instructionPointer: Int, memoryContents: MemoryContents) = {
    val positionModeParam1 = memoryContents.getPositionMode(1)
    val positionModeParam2 = memoryContents.getPositionMode(2)
    val newValue = memory.getValueForParameter(instructionPointer + 1, positionModeParam1) *
      memory.getValueForParameter(instructionPointer + 2, positionModeParam2)
    memory.setValue(instructionPointer + 3, newValue)
  }

  private def sumOp(memory: Memory, instructionPointer: Int, memoryContents: MemoryContents) = {
    val positionModeParam1 = memoryContents.getPositionMode(1)
    val positionModeParam2 = memoryContents.getPositionMode(2)
    val newValue = memory.getValueForParameter(instructionPointer + 1, positionModeParam1) +
      memory.getValueForParameter(instructionPointer + 2, positionModeParam2)
    memory.setValue(instructionPointer + 3, newValue)
  }

  private def lessThanOp(memory: Memory, instructionPointer: Int, memoryContents: MemoryContents) = {
    val positionMode1 = memoryContents.getPositionMode(1)
    val positionMode2 = memoryContents.getPositionMode(2)
    val firstParam = memory.getValueForParameter(instructionPointer + 1, positionMode1)
    val secondParam = memory.getValueForParameter(instructionPointer + 2, positionMode2)
    if (firstParam < secondParam) {
      memory.setValue(instructionPointer + 3, 1)
    } else {
      memory.setValue(instructionPointer + 3, 0)
    }
  }

  private def equalsOp(memory: Memory, instructionPointer: Int, memoryContents: MemoryContents) = {
    val positionMode1 = memoryContents.getPositionMode(1)
    val positionMode2 = memoryContents.getPositionMode(2)
    val firstParam = memory.getValueForParameter(instructionPointer + 1, positionMode1)
    val secondParam = memory.getValueForParameter(instructionPointer + 2, positionMode2)
    if (firstParam == secondParam) {
      memory.setValue(instructionPointer + 3, 1)
    } else {
      memory.setValue(instructionPointer + 3, 0)
    }
  }
}