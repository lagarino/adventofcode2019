
case class Coordenate(x: Int, y: Int) {
  def distanceTo(origin: Coordenate): Int = {
    (x - origin.x).abs + (y - origin.y).abs
  }
}

case class Angle(x: Int, y: Int) {
  def calculateAngle(): Double = {
    val angle = math.atan2(x, y) * 180 / math.Pi + 90
    if (angle < 0) angle + 360
    else angle
  }
}

object Angle {
  def apply(origin: Coordenate, target: Coordenate): Angle = {
    if (origin == target)
      Angle(0, 0)
    else {
      val diffX = target.x - origin.x
      val diffY = target.y - origin.y
      val gcdValue = gcd(diffX, diffY).abs
      Angle(diffX / gcdValue, diffY / gcdValue)
    }
  }

  @scala.annotation.tailrec
  private def gcd(x: Int, y: Int): Int =
    if (y == 0) x
    else gcd(y, x % y)
}

case class Day10(input: String) {
  val map: Array[Array[Char]] = input.linesIterator.map(line => line.toCharArray).toArray
  val asteroids: Seq[Coordenate] = for {
    x <- map.head.indices
    y <- map.indices
    if map(y)(x) == '#'
  } yield Coordenate(y, x)

  def calculateBetterSpot(): (Coordenate, Int) = {
    val numberOfVisible = asteroids.map(numberOfVisibleAsteorids)
    val max = numberOfVisible.max
    (asteroids(numberOfVisible.indexOf(max)), max)
  }

  def numberOfVisibleAsteorids(origin: Coordenate): Int = {
    asteroids.map(asteroid => Angle(origin, asteroid)).toSet.size - 1
  }

  def vaporizedAsteroidsInOrderSimpler(): Seq[Coordenate] = {
    val laserStation = calculateBetterSpot()._1

    def inner(remainingAsteroids: Seq[Coordenate]): List[Coordenate] = {
      val visibleAsteroids = getVisibleAsteroidsInClockwiseOrder(laserStation, remainingAsteroids).toList
      if (visibleAsteroids.isEmpty) visibleAsteroids
      else {
        visibleAsteroids ++ inner(remainingAsteroids.filterNot(a => visibleAsteroids.contains(a)))
      }
    }

    inner(asteroids)
  }

  private def getVisibleAsteroidsInClockwiseOrder(origin: Coordenate, remainingAsteroids: Seq[Coordenate]): Seq[Coordenate] = {
    val visibleAngles = remainingAsteroids
      .map(asteroid => Angle(origin, asteroid))
      .distinct
      .sortBy(angle => angle.calculateAngle())
    visibleAngles.flatMap(angle => remainingAsteroids.sortBy(_.distanceTo(origin)).find(asteroid => Angle(origin, asteroid) == angle))
  }
}
