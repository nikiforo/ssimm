package name.nikiforo.ssimm.filter

import name.nikiforo.ssimm.{Image, ResultContainer, _}

/**
  * Transforms each pixel with transform function */
trait SinglePixelFilter extends Filter {
  protected def transform(pixel: Pixel): Pixel
  override def apply(image: Image)(implicit resultContainer: ResultContainer): Image = {
    val mutableImg = resultContainer(image.width, image.height)
    for {
      x <- 0 until image.width
      y <- 0 until image.height
    } {
      mutableImg.set(x, y, transform(image(x, y)))
    }

    mutableImg
  }
}

object NoFilter extends SinglePixelFilter {
  override def transform(pixel: Pixel) = pixel
}

object RedComponentFilter extends SinglePixelFilter {
  override def transform(pixel: Pixel) = getPixel(pixel.alpha, pixel.red, 0, 0)
}

object GreenComponentFilter extends SinglePixelFilter {
  override def transform(pixel: Pixel) = getPixel(pixel.alpha, 0, pixel.green, 0)
}

object BlueComponentFilter extends SinglePixelFilter {
  override def transform(pixel: Pixel) = getPixel(pixel.alpha, 0, 0, pixel.blue)
}

object GrayLightnessFilter extends SinglePixelFilter {
  def max(a: Int, b: Int, c: Int) = math.max(math.max(a, b), c)
  def min(a: Int, b: Int, c: Int) = math.min(math.min(a, b), c)

  override def transform(pixel: Pixel) = {
    val (a, r, g, b) = pixel.argb
    val avg = (max(r, g, b) + min(r, g, b)) / 2
    getPixel(a, avg, avg, avg)
  }
}

object GrayAverageFilter extends SinglePixelFilter {
  override def transform(pixel: Pixel) = {
    val (a, r, g, b) = pixel.argb
    val avg = (r + g + b) / 3
    getPixel(a, avg, avg, avg)
  }
}

object GrayLuminosityFilter extends SinglePixelFilter {
  override def transform(pixel: Pixel) = {
    val (a, r, g, b) = pixel.argb
    val avg = (0.21 * r + 0.72 * g + 0.07 * b).toInt
    getPixel(a, avg, avg, avg)
  }
}
