package name.nikiforo.ssimm.filter

import name.nikiforo.ssimm._

/**
  * Filter represents computation, that can be applied to an image, producing another Image
  * */
trait Filter {
  def apply(image: Image)(implicit resultContainer: ResultContainer): Image
}

trait KMeansFilter extends Filter {
  protected def clusterAmount: Int

  override def apply(image: Image)(implicit resultContainer: ResultContainer): Image = {
    val seq = for {
      x <- 0 until image.width
      y <- 0 until image.height
      pixel = image(x,y)
      (a, r, g, b) = pixel.argb
      colorComponent <- Seq(a, r, g, b)
    } yield colorComponent.asInstanceOf[Int]

    val x = KmeansClasterizator.clasterize(clusterAmount, seq.toArray)

    val getValue = (for
    {
      cluster <- x
      c = cluster.center
      a = c.a; r = c.r; g = c.g; b = c.b
      pixel = getPixel(a.toInt, r.toInt, g.toInt, b.toInt)
      i <- 0 until (cluster.points.length / 4)
      ind = i * 4
      aPoint = cluster.points(ind)
      rPoint = cluster.points(ind + 1)
      gPoint = cluster.points(ind + 2)
      bPoint = cluster.points(ind + 3)
    } yield getPixel(aPoint, rPoint, gPoint, bPoint) -> pixel).toMap

    val mutableImg = resultContainer(image.width, image.height)
    for {
      x <- 0 until image.width
      y <- 0 until image.height
    } {
      mutableImg.set(x, y, getValue(image(x, y)))
    }

    mutableImg
  }
}