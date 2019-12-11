import org.scalatest.{FlatSpec, Matchers}

class day11Tests extends FlatSpec with Matchers {

  "The first part" should "work for the input" in {
    val sut = Robot("3,8,1005,8,334,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,28,2,1108,5,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,55,1,102,18,10,1,2,5,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,1001,8,0,84,1,106,11,10,2,1008,6,10,1,4,4,10,1006,0,55,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,121,1,107,9,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,101,0,8,147,2,1002,4,10,2,104,18,10,1,107,16,10,1,108,8,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,102,1,8,185,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,101,0,8,208,2,1009,16,10,1006,0,7,1006,0,18,1,1105,8,10,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,101,0,8,243,2,1105,20,10,2,106,10,10,1006,0,67,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1001,8,0,276,2,1103,5,10,2,1104,7,10,1006,0,35,2,1105,3,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,1002,8,1,314,101,1,9,9,1007,9,1097,10,1005,10,15,99,109,656,104,0,104,1,21102,936995824532,1,1,21101,0,351,0,1105,1,455,21102,1,387508445964,1,21102,362,1,0,1106,0,455,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,1,235244973059,1,21101,409,0,0,1106,0,455,21102,179410541659,1,1,21101,0,420,0,1105,1,455,3,10,104,0,104,0,3,10,104,0,104,0,21101,868402070292,0,1,21102,1,443,0,1106,0,455,21102,1,709584749324,1,21102,454,1,0,1106,0,455,99,109,2,22102,1,-1,1,21101,40,0,2,21102,486,1,3,21101,0,476,0,1106,0,519,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,481,482,497,4,0,1001,481,1,481,108,4,481,10,1006,10,513,1101,0,0,481,109,-2,2106,0,0,0,109,4,2102,1,-1,518,1207,-3,0,10,1006,10,536,21102,0,1,-3,21202,-3,1,1,22102,1,-2,2,21102,1,1,3,21102,555,1,0,1106,0,560,109,-4,2106,0,0,109,5,1207,-3,1,10,1006,10,583,2207,-4,-2,10,1006,10,583,21201,-4,0,-4,1106,0,651,21201,-4,0,1,21201,-3,-1,2,21202,-2,2,3,21102,602,1,0,1106,0,560,22102,1,1,-4,21101,0,1,-1,2207,-4,-2,10,1006,10,621,21102,0,1,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,643,21201,-1,0,1,21102,643,1,0,106,0,518,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0")
    sut.paint()
    sut.numberOfPanels() shouldBe 2016
  }

  "The second part" should "work for the input" in {
    val sut = Robot("3,8,1005,8,334,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,28,2,1108,5,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,55,1,102,18,10,1,2,5,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,1001,8,0,84,1,106,11,10,2,1008,6,10,1,4,4,10,1006,0,55,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,121,1,107,9,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,101,0,8,147,2,1002,4,10,2,104,18,10,1,107,16,10,1,108,8,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,102,1,8,185,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,101,0,8,208,2,1009,16,10,1006,0,7,1006,0,18,1,1105,8,10,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,101,0,8,243,2,1105,20,10,2,106,10,10,1006,0,67,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1001,8,0,276,2,1103,5,10,2,1104,7,10,1006,0,35,2,1105,3,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,1002,8,1,314,101,1,9,9,1007,9,1097,10,1005,10,15,99,109,656,104,0,104,1,21102,936995824532,1,1,21101,0,351,0,1105,1,455,21102,1,387508445964,1,21102,362,1,0,1106,0,455,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,1,235244973059,1,21101,409,0,0,1106,0,455,21102,179410541659,1,1,21101,0,420,0,1105,1,455,3,10,104,0,104,0,3,10,104,0,104,0,21101,868402070292,0,1,21102,1,443,0,1106,0,455,21102,1,709584749324,1,21102,454,1,0,1106,0,455,99,109,2,22102,1,-1,1,21101,40,0,2,21102,486,1,3,21101,0,476,0,1106,0,519,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,481,482,497,4,0,1001,481,1,481,108,4,481,10,1006,10,513,1101,0,0,481,109,-2,2106,0,0,0,109,4,2102,1,-1,518,1207,-3,0,10,1006,10,536,21102,0,1,-3,21202,-3,1,1,22102,1,-2,2,21102,1,1,3,21102,555,1,0,1106,0,560,109,-4,2106,0,0,109,5,1207,-3,1,10,1006,10,583,2207,-4,-2,10,1006,10,583,21201,-4,0,-4,1106,0,651,21201,-4,0,1,21201,-3,-1,2,21202,-2,2,3,21102,602,1,0,1106,0,560,22102,1,1,-4,21101,0,1,-1,2207,-4,-2,10,1006,10,621,21102,0,1,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,643,21201,-1,0,1,21102,643,1,0,106,0,518,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0")
    sut.paintedPanels(Point(0, 0)) = 1
    sut.paint()
    sut.drawImage()
  }
}
