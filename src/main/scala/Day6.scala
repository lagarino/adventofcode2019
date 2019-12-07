
case class Day6(map: String) {
  val planetsAndParents: Map[String, String] = map.linesIterator.foldLeft(Map[String, String]()) { (planets, newRelation) =>
    val parts = newRelation.split("\\)")
    planets + (parts(1) -> parts(0))
  }

  def countOrbits(): Int = {
    planetsAndParents.foldLeft(Map[String, Int]()) { (numberOfOrbitsCache, planet) =>
      val orbits = calculateNumberOfOrbits(numberOfOrbitsCache, planet._1)
      numberOfOrbitsCache + (planet._1 -> orbits)
    }.values.sum
  }

  def findOrbitsBetweenPlanets(planet1: String, planet2: String): Int = {
    val parentsOfPlanet1 = findParentsOfPlanet(planet1)
    val parentsOfPlanet2 = findParentsOfPlanet(planet2)
    val firstCommonAncenstor = parentsOfPlanet1.intersect(parentsOfPlanet2).head
    parentsOfPlanet1.indexOf(firstCommonAncenstor) + parentsOfPlanet2.indexOf(firstCommonAncenstor)
  }

  private def calculateNumberOfOrbits(cache: Map[String, Int], planet: String): Int = {
    cache.get(planet) match {
      case None =>
        planetsAndParents.get(planet) match {
          case None => 0
          case Some(parent) => 1 + calculateNumberOfOrbits(cache, parent)
        }
      case Some(orbits) =>
        orbits
    }
  }

  private def findParentsOfPlanet(planet: String): Vector[String] = {
    planetsAndParents.get(planet) match {
      case None => Vector.empty
      case Some(parent) => parent +: findParentsOfPlanet(parent)
    }
  }

}