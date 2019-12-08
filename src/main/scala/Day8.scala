
case class Day8(code: String, width: Int, height: Int) {
  val layers: Vector[String] = code.grouped(width*height).toVector
  val combinedLayer: Array[Char] = combineLayers()

  def findLayerWithLessZeroes(): String = {
    layers.minBy(layer => layer.count(_ == '0'))
  }

  def calculateHashOfLayer(layer: String): Int =
    layer.count(_ == '1') * layer.count(_ == '2')

  private def combineLayers(): Array[Char] = {
    (0 until (width * height)).map { index =>
      layers.foldLeft('2') { (previousPixel, newLayer) =>
        val newPixel = newLayer.charAt(index)
        (previousPixel, newPixel) match {
          case ('2', _) => newPixel
          case ('0', _) => '0'
          case ('1', _) => '1'
        }
      }
    }.toArray
  }

  def printImage(): Unit = {
    val rows: Vector[Array[Char]] = combinedLayer.grouped(width).toVector
    rows.foreach(r => println(r.mkString("")))
  }

}
