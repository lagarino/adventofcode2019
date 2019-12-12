import org.scalatest.{FlatSpec, Matchers}

class day12Tests extends FlatSpec with Matchers {

  "The first part" should "work for the input" in {
    val system = MoonSystem("<x=-9, y=10, z=-1>\n<x=-14, y=-8, z=14>\n<x=1, y=5, z=6>\n<x=-19, y=7, z=8>")
      .advanceTimeSlots(1000)
    system.calculateTotalEnergy() shouldBe 8538
  }

  it should "work for the first example" in {
    val system = MoonSystem("<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>")
      .advanceTimeSlots(10)
    system.calculateTotalEnergy() shouldBe 179
  }

  "The second part" should "work for the first example" in {
    val system = MoonSystem("<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>")
    system.cycleForX() shouldBe 18
    system.cycleForY() shouldBe 28
    system.cycleForZ() shouldBe 44
  }

  it should "work for the second example" in {
    val system = MoonSystem("<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>")
    system.cycleForX() shouldBe 2028
    system.cycleForY() shouldBe 5898
    system.cycleForZ() shouldBe 4702
  }

  it should "work for the input" in {
    val system = MoonSystem("<x=-9, y=10, z=-1>\n<x=-14, y=-8, z=14>\n<x=1, y=5, z=6>\n<x=-19, y=7, z=8>")
    system.cycleForX() shouldBe 161428
    system.cycleForY() shouldBe 231614
    system.cycleForZ() shouldBe 108344
    system.totalCycle() shouldBe BigInt("506359021038056")
  }
}
