package name.nikiforo.ssimm.filter

import name.nikiforo.ssimm._

/**
  * Filter represents computation, that can be applied to an image, producing another Image
  * */
trait Filter {
  def apply(image: Image)(implicit resultContainer: ResultContainer): Image
}

object KMeansFilterHelper {
  def convertToKMeansFormat(image: Image): Array[Int] = {
    val seq = for {
      x <- 0 until image.width
      y <- 0 until image.height
      pixel = image(x,y)
      (a, r, g, b) = pixel.argb
      colorComponent <- Seq(r, g, b)
    } yield colorComponent.asInstanceOf[Int]

    seq.toArray
  }
}

class KMeansFilterHelper(centers: Seq[Center]) extends SinglePixelFilter {
  override def transform(pixel: Pixel) = {
    val (a, r, g, b) = pixel.argb
    val closest = centers.minBy(_.manhDist(r, g, b))
    getPixel(a, closest.rInt, closest.gInt, closest.bInt)
  }
}

class KMeansFilter(clusterAmount: Int) extends Filter {
  override def apply(image: Image)(implicit resultContainer: ResultContainer): Image = {
    val kMeansFormattedImage = KMeansFilterHelper.convertToKMeansFormat(image)
    val centers = KmeansClusterer.cluster(clusterAmount, kMeansFormattedImage)
    image.applyFilter(new KMeansFilterHelper(centers))
  }
}

class MedianFilter(clusterAmount: Int) extends Filter {
  override def apply(image: Image)(implicit resultContainer: ResultContainer): Image = {
    val kMeansFormattedImage = KMeansFilterHelper.convertToKMeansFormat(image)
    val centers = KMeansClusterHelper.median(clusterAmount, kMeansFormattedImage)
    image.applyFilter(new KMeansFilterHelper(centers))
  }
}

class SmoothFilter(colorDiffThreshold: Int, regionSizeThreshold: Int) extends Filter {
  override def apply(image: Image)(implicit resultContainer: ResultContainer): Image = {
    Smoother.smooth(image, colorDiffThreshold, regionSizeThreshold)
  }
}