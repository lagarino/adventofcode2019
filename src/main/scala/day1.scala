
object day1 {

  def calculateFuel(numbers: String): Int = {
    numbers.linesIterator.foldLeft(0) { (totalFuel, weight) =>
      totalFuel + calculateFuelFromWeight(weight.toInt)
    }
  }

  private def calculateFuelFromWeight(weight: Int): Int = {
    val fuel = weight.toInt/3 - 2
    if (fuel > 0) fuel
    else 0
  }

  def calculateRecursiveFuel(numbers: String): Int = {
    numbers.linesIterator.map(_.toInt).map(calculateRecursiveFuelForModule).sum
  }

  private def calculateRecursiveFuelForModule(weight: Int): Int = {
    val fuel = calculateFuelFromWeight(weight)
    if (fuel > 0) fuel + calculateRecursiveFuelForModule(fuel)
    else 0
  }
}