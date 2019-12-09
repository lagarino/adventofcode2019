import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class MemoryContents(contents: String) {
  val operation: Int = contents.takeRight(2).toInt
  def getPositionMode(order: Int): Int =
    if (contents.length >= order + 2) contents.takeRight(order + 2).head.toString.toInt else 0
}

case class Memory(var contents: mutable.Map[Long, Long]) {
  private var relativeBase: Long = 0

  def getContent(pointer: Long): MemoryContents = {
    MemoryContents(contents(pointer).toString)
  }

  def increaseRelativeBase(diff: Long): Unit = {
    relativeBase += diff
  }

  def setValue(pointer: Long, value: Long, positionMode: Int = 0): Unit = {
    val index = positionMode match {
      case 0 => contents(pointer)                 // position mode
      case _ => relativeBase + contents(pointer)  // relative mode
    }
    contents(index) = value
  }

  def getValueForParameter(pointer: Long, positionMode: Int = 0): Long = {
    val index = positionMode match {
      case 0 => contents(pointer)                 // position mode
      case 1 => pointer                           // direct mode
      case _ => relativeBase + contents(pointer)  // relative mode
    }
    contents.getOrElse(index, 0L)
  }
}

case class IntcodeComputer(phase: Long, memoryContents: String, stopOnOutput: Boolean = false) {
  private val memory: mutable.Map[Long, Long] = mutable.Map()

  memoryContents.split(",").foldLeft(0L) { case(index, content) =>
    memory.put(index, content.toLong)
    index + 1
  }

  private val inputsQueue = mutable.Queue[Long]()
  inputsQueue.enqueue(phase)
  private var outputs = ListBuffer[Long]()
  private var instructionPointerState: Long = 0

  def executeProgram(inputs: Long*): Vector[Long] = {
    outputs = ListBuffer[Long]()
    inputsQueue.enqueueAll(inputs)
    executeInstruction(Memory(memory), instructionPointerState)
    outputs.toVector
  }

  @scala.annotation.tailrec
  private def executeInstruction(memory: Memory, instructionPointer: Long): Memory = {
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
        inputOp(memory, instructionPointer, memoryContents)
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
      case 9 =>
        adjustRelativeBase(memory, instructionPointer, memoryContents)
        executeInstruction(memory, instructionPointer+2)
    }
  }

  private def adjustRelativeBase(memory: Memory, instructionPointer: Long, memoryContents: MemoryContents): Unit = {
    val positionMode = memoryContents.getPositionMode(1)
    val diff = memory.getValueForParameter(instructionPointer + 1, positionMode)
    memory.increaseRelativeBase(diff)
  }

  private def outputOp(memory: Memory, instructionPointer: Long, memoryContents: MemoryContents) = {
    val positionMode = memoryContents.getPositionMode(1)
    val outputValue = memory.getValueForParameter(instructionPointer + 1, positionMode)
    outputs.addOne(outputValue)
  }

  private def inputOp(memory: Memory, instructionPointer: Long, memoryContents: MemoryContents): Unit = {
    val positionMode = memoryContents.getPositionMode(1)
    val inputValue = inputsQueue.dequeue()
    memory.setValue(instructionPointer + 1, inputValue, positionMode)
  }

  private def jumpIfFalseOp(memory: Memory, instructionPointer: Long, memoryContents: MemoryContents) = {
    val positionMode1 = memoryContents.getPositionMode(1)
    val positionMode2 = memoryContents.getPositionMode(2)
    if (memory.getValueForParameter(instructionPointer + 1, positionMode1) == 0) {
      val newInstructionPointer = memory.getValueForParameter(instructionPointer + 2, positionMode2)
      executeInstruction(memory, newInstructionPointer)
    } else {
      executeInstruction(memory, instructionPointer + 3)
    }
  }

  private def jumpIfTrueOp(memory: Memory, instructionPointer: Long, memoryContents: MemoryContents): Memory = {
    val positionMode1 = memoryContents.getPositionMode(1)
    val positionMode2 = memoryContents.getPositionMode(2)
    if (memory.getValueForParameter(instructionPointer + 1, positionMode1) != 0) {
      val newInstructionPointer = memory.getValueForParameter(instructionPointer + 2, positionMode2)
      executeInstruction(memory, newInstructionPointer)
    } else {
      executeInstruction(memory, instructionPointer + 3)
    }
  }

  private def timesOp(memory: Memory, instructionPointer: Long, memoryContents: MemoryContents): Unit = {
    val positionModeParam1 = memoryContents.getPositionMode(1)
    val positionModeParam2 = memoryContents.getPositionMode(2)
    val positionModeParam3 = memoryContents.getPositionMode(3)
    val newValue = memory.getValueForParameter(instructionPointer + 1, positionModeParam1) *
      memory.getValueForParameter(instructionPointer + 2, positionModeParam2)
    memory.setValue(instructionPointer + 3, newValue, positionModeParam3)
  }

  private def sumOp(memory: Memory, instructionPointer: Long, memoryContents: MemoryContents): Unit = {
    val positionModeParam1 = memoryContents.getPositionMode(1)
    val positionModeParam2 = memoryContents.getPositionMode(2)
    val positionModeParam3 = memoryContents.getPositionMode(3)
    val newValue = memory.getValueForParameter(instructionPointer + 1, positionModeParam1) +
      memory.getValueForParameter(instructionPointer + 2, positionModeParam2)
    memory.setValue(instructionPointer + 3, newValue, positionModeParam3)
  }

  private def lessThanOp(memory: Memory, instructionPointer: Long, memoryContents: MemoryContents): Unit = {
    val positionMode1 = memoryContents.getPositionMode(1)
    val positionMode2 = memoryContents.getPositionMode(2)
    val positionMode3 = memoryContents.getPositionMode(3)
    val firstParam = memory.getValueForParameter(instructionPointer + 1, positionMode1)
    val secondParam = memory.getValueForParameter(instructionPointer + 2, positionMode2)
    if (firstParam < secondParam) {
      memory.setValue(instructionPointer + 3, 1, positionMode3)
    } else {
      memory.setValue(instructionPointer + 3, 0, positionMode3)
    }
  }

  private def equalsOp(memory: Memory, instructionPointer: Long, memoryContents: MemoryContents): Unit = {
    val positionMode1 = memoryContents.getPositionMode(1)
    val positionMode2 = memoryContents.getPositionMode(2)
    val positionMode3 = memoryContents.getPositionMode(3)
    val firstParam = memory.getValueForParameter(instructionPointer + 1, positionMode1)
    val secondParam = memory.getValueForParameter(instructionPointer + 2, positionMode2)
    if (firstParam == secondParam) {
      memory.setValue(instructionPointer + 3, 1, positionMode3)
    } else {
      memory.setValue(instructionPointer + 3, 0, positionMode3)
    }
  }
}