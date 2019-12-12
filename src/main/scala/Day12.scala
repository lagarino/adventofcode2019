
case class Moon(index: Int, x: Int, y: Int, z: Int, velocityX: Int = 0, velocityY: Int = 0, velocityZ: Int = 0) {
  def applyGravity(anotherMoon: Moon): Moon = {
    val newVelocityX = if (x > anotherMoon.x) velocityX - 1 else if (x < anotherMoon.x) velocityX + 1 else velocityX
    val newVelocityY = if (y > anotherMoon.y) velocityY - 1 else if (y < anotherMoon.y) velocityY + 1 else velocityY
    val newVelocityZ = if (z > anotherMoon.z) velocityZ - 1 else if (z < anotherMoon.z) velocityZ + 1 else velocityZ

    copy(velocityX = newVelocityX, velocityY = newVelocityY, velocityZ = newVelocityZ)
  }

  def move(): Moon = {
    copy(x = x + velocityX, y = y + velocityY, z = z + velocityZ)
  }

  def potEnergy(): Int =
    x.abs + y.abs + z.abs

  def kinEnergy(): Int =
    velocityX.abs + velocityY.abs + velocityZ.abs

  def totalEnergy(): Int = potEnergy() * kinEnergy()
}

case class MoonSystem(moons: Vector[Moon]) {
  def advanceTimeSlots(numberOfSlots: Int): MoonSystem = {
    (0 until numberOfSlots).foldLeft(this) { (moonSystem, _) =>
      moonSystem.advanceTimeSlot()
    }
  }

  def advanceTimeSlot(): MoonSystem = {
    val newMoons = moons.map { moon =>
      moons.foldLeft(moon) { (acc, anotherMoon) =>
        if (anotherMoon != moon) acc.applyGravity(anotherMoon)
        else acc
      }.move()
    }
    MoonSystem(newMoons)
  }

  def calculateTotalEnergy(): Int = {
    moons.foldLeft(0) { (acc, moon) =>
      acc + moon.totalEnergy()
    }
  }

  def cycleForX(): Int = {
    cycleForCoordinate(this, isXEqual)
  }

  def cycleForY(): Int = {
    cycleForCoordinate(this, isYEqual)
  }

  def cycleForZ(): Int = {
    cycleForCoordinate(this, isZEqual)
  }

  def totalCycle(): BigInt = {
    def lcm(list: Seq[BigInt]): BigInt =
      list.foldLeft(1: BigInt) { (a, b) =>
        b * a / Stream.iterate((a, b)) {
          case (x, y) => (y, x % y)
        }.dropWhile(_._2 != 0).head._1.abs
      }

    lcm((cycleForX() :: cycleForY() :: cycleForZ() :: Nil).map(BigInt(_)))
  }

  def isXEqual(anotherSystem: MoonSystem): Boolean = {
    anotherSystem.moons.map(_.x) == moons.map(_.x) && anotherSystem.moons.map(_.velocityX) == moons.map(_.velocityX)
  }

  def isYEqual(anotherSystem: MoonSystem): Boolean = {
    anotherSystem.moons.map(_.y) == moons.map(_.y) && anotherSystem.moons.map(_.velocityY) == moons.map(_.velocityY)
  }

  def isZEqual(anotherSystem: MoonSystem): Boolean = {
    anotherSystem.moons.map(_.z) == moons.map(_.z) && anotherSystem.moons.map(_.velocityZ) == moons.map(_.velocityZ)
  }

  def cycleForCoordinate(lastSystem: MoonSystem, isEqual: (MoonSystem) => Boolean): Int = {
    var counter = 1
    var anotherSystem = lastSystem.advanceTimeSlot()
    while (!isEqual(anotherSystem)) {
      anotherSystem = anotherSystem.advanceTimeSlot()
      counter += 1
    }

    counter
  }
}

object MoonSystem {
  def apply(input: String): MoonSystem = {
    val moons = input.linesIterator.map(
      _.stripPrefix("<")
        .stripSuffix(">")
        .split(",")
        .map(coord => coord.split("=")(1).toInt)
    ).zipWithIndex.map { case (coords, index) =>
      Moon(index, coords(0), coords(1), coords(2))
    }.toVector
    MoonSystem(moons)
  }
}