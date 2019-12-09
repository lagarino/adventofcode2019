
case class Day7(programToRun: String) {

  def calculateMaxValueSentToThrusters(): Int = {
    val possibleValues = 0 to 4
    computePermutations(Vector.empty, possibleValues)
      .map(combination => combination.map(phase => IntcodeComputer(phase, programToRun)))
      .flatMap(combination => calculateValueSentToThrusters(0, combination: _*)).max
  }

  def calculateValueSentToThrusters(input: Int, computers: IntcodeComputer*): Option[Int] = {
    computers.foldLeft(Option(input)) { (phaseInput, phaseComputer) =>
      phaseInput match {
        case None => None
        case Some(pI) => phaseComputer.executeProgram(pI).lastOption
      }
    }
  }

  def calculateMaxValueSentToThrustersLooped(): Int = {
    val possibleValues = 5 to 9
    computePermutations(Vector.empty, possibleValues)
      .map(combination => calculateValueSentToThrustersLooped(combination: _*)).max
  }

  def calculateValueSentToThrustersLooped(phaseSettings: Int*): Int = {
    val computers = phaseSettings.map(phase => IntcodeComputer(phase, programToRun, stopOnOutput = true))

    @scala.annotation.tailrec
    def feedBackOutput(fedBackValue: Int): Int = {
      calculateValueSentToThrusters(fedBackValue, computers:_*) match {
        case None => fedBackValue
        case Some(newFedBackValue) => feedBackOutput(newFedBackValue)
      }
    }

    feedBackOutput(0)
  }

  private def computePermutations(prefix: Vector[Int], possibleValues: IndexedSeq[Int]): IndexedSeq[Vector[Int]] = {
    if (possibleValues.isEmpty) Vector(prefix)
    else possibleValues.flatMap { value => computePermutations(prefix :+ value, possibleValues.filterNot(_.equals(value))) }
  }
}
