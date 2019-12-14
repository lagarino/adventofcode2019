import scala.collection.mutable

case class RequiredChemical(name: String, quantity: Long)

object RequiredChemical {
  def apply(input: String): RequiredChemical = {
    val parts = input.trim.split(" ")
    RequiredChemical(parts(1), parts(0).toInt)
  }
}

case class Day14(reactionsInput: String) {
  private val reactions = {
    reactionsInput.linesIterator.map { line =>
      val parts = line.split("=>")
      RequiredChemical(parts(1)) -> parts(0).split(",").map(RequiredChemical(_))
    }.toMap
  }

  def calculateHowManyFuel(ore: RequiredChemical, startingPoint: Long = 0): Long = {
    var fuel = startingPoint
    var extraChemicals: ChemicalElements = ChemicalElements(mutable.Map[String, Long]())
    do {
      fuel += 1
      extraChemicals = calculateRequiredOre(ChemicalElements(mutable.Map("FUEL" -> fuel), mutable.Map(ore.name -> ore.quantity)))
    } while (extraChemicals.requiredChemicals.isEmpty)
    fuel - 1
  }

  def calculateRequiredOre(requiredChemicals: ChemicalElements): ChemicalElements = {

    val surplus = requiredChemicals.surplusChemicals.clone()
    val required = requiredChemicals.requiredChemicals.clone()

    def removeFromRequired(name: String, quantity: Long) = {
      required.getOrElse(name, 0L) - quantity match {
        case 0 => required.remove(name)
        case remaining => required.put(name, remaining)
      }
    }

    def removeFromSurplus(name: String, quantity: Long) = {
      surplus.getOrElse(name, 0L) - quantity match {
        case 0 => surplus.remove(name)
        case remaining => surplus.put(name, remaining)
      }
    }

    def reduceRequiredFromSurplus(name: String, requiredQuantity: Long): Long = {
      surplus.find { sur => sur._1 == name } map { case (_, surQuantity) =>
        val usedQuantity =
          if (surQuantity > requiredQuantity) requiredQuantity
          else surQuantity

        removeFromRequired(name, usedQuantity)
        removeFromSurplus(name, usedQuantity)
        usedQuantity
      } getOrElse 0
    }

    required.foreach { chemical =>
      if (chemical._1 != "ORE") {
        val availableFromRequired = reduceRequiredFromSurplus(chemical._1, chemical._2)
        val missingQuantity = chemical._2 - availableFromRequired
        if (missingQuantity > 0) {
          val formulae = reactions.find(_._1.name == chemical._1).get
          val multiplier =
            if (formulae._1.quantity >= missingQuantity) 1
            else if (missingQuantity % formulae._1.quantity == 0) missingQuantity / formulae._1.quantity
            else 1 + (missingQuantity / formulae._1.quantity)

          val totalGenerated = multiplier * formulae._1.quantity
          val totalOverflow = totalGenerated - missingQuantity

          formulae._2.flatMap { requiredChemical =>
            required.put(requiredChemical.name, multiplier * requiredChemical.quantity + required.getOrElse(requiredChemical.name, 0L))
          }.toList

          removeFromRequired(chemical._1, missingQuantity)

          required.foreach { case (name, quantity) =>
            reduceRequiredFromSurplus(name, quantity)
          }

          if (totalOverflow > 0) surplus.put(chemical._1, totalOverflow + surplus.getOrElse(chemical._1, 0L))
        }
      }
    }

    val nextIterationRequirements = ChemicalElements(required, surplus)
    if (nextIterationRequirements.requiredChemicals.isEmpty || (nextIterationRequirements.requiredChemicals.size == 1 && nextIterationRequirements.requiredChemicals.head._1 == "ORE"))
      nextIterationRequirements
    else
      calculateRequiredOre(ChemicalElements(required, surplus))
  }
}

case class ChemicalElements(requiredChemicals: mutable.Map[String, Long], surplusChemicals: mutable.Map[String, Long] = mutable.Map[String, Long]())
