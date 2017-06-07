package name.nikiforo.immortalcombat

trait Filter {
  def apply(image: Image)(implicit resultContainer: ResultContainer): Image
}

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

object GrayLightnessFilter extends SinglePixelFilter {
  def max(a: Int, b: Int, c: Int) = math.max(math.max(a, b), c)
  def min(a: Int, b: Int, c: Int) = math.min(math.min(a, b), c)
  override def transform(pixel: Pixel) = {
    val avg = (max(pixel.red, pixel.green, pixel.blue) + min(pixel.red, pixel.green, pixel.blue)) / 2
    getPixel(pixel.alpha, avg, avg, avg)
  }
}

object GrayAverageFilter extends SinglePixelFilter {
  override def transform(pixel: Pixel) = {
    val avg = (pixel.red + pixel.green + pixel.blue) / 3
    getPixel(pixel.alpha, avg, avg, avg)
  }
}

object GrayLuminosityFilter extends SinglePixelFilter {
  override def transform(pixel: Pixel) = {
    val avg = (0.21 * pixel.red + 0.72 * pixel.green + 0.07 * pixel.blue).toInt
    getPixel(pixel.alpha, avg, avg, avg)
  }
}
