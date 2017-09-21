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
    } yield image(x, y)

    val x = KmeansClasterizator.clasterize(clusterAmount, seq.toList.map(_.argb))

    val getValue = (for
    {
      cluster <- x
      (a, r, g, b) = cluster.center
      pixel = getPixel(a.toInt, r.toInt, g.toInt, b.toInt)
      (aPoint, rPoint, gPoint, bPoint) <- cluster.points
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