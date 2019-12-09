
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
        previousPixel match {
          case '2' => newLayer.charAt(index)
          case other => other
        }
      }
    }.toArray
  }

  def printImage(): Unit = {
    val rows: Vector[Array[Char]] = combinedLayer.grouped(width).toVector
    rows.foreach(r => println(r.mkString("")))
  }

}
