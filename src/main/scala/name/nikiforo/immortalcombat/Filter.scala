package name.nikiforo.immortalcombat

/**
  * Filter represents computation, that can be applied to an image, producing another Image
  * */
trait Filter {
  def apply(image: Image)(implicit resultContainer: ResultContainer): Image
}

/**
  * Easy, naive and slow convolution filter, that has a kernel, that is applied to a pixel with neighbours.
  * Suppose this filter has a n*m kernel and applied to an image x*y, then a result image will have x-(n-1) * y-(m-1) size
  * */
trait SlowKernelFilter extends Filter {
  protected def Kernel: Array[Array[Int]]
  protected def Divisor: Int
  protected def MiddleIndexX: Int // Index of the center, equals to "border" length
  protected def MiddleIndexY: Int // Index of the center, equals to "border" length

  def apply(image: Image)(implicit resultContainer: ResultContainer): Image = {
    require(image.width > MiddleIndexX*2 && image.height > MiddleIndexY*2)
    val mutableImg = resultContainer(image.width - MiddleIndexX*2, image.height - MiddleIndexY*2)

    for {
      x <- MiddleIndexX until image.width - MiddleIndexX
      y <- MiddleIndexY until image.height - MiddleIndexY
    } {
      var sumA = 0
      var sumR = 0
      var sumG = 0
      var sumB = 0

      for {
        kernelX <- -1 * MiddleIndexX to MiddleIndexX
        kernelY <- -1 * MiddleIndexY to MiddleIndexY
      } {
        val pixel = image(x + kernelX, y + kernelY)
        val (a, r, g, b) = pixel.argb

        val ky = kernelY + MiddleIndexY
        val kx = kernelX + MiddleIndexX
        sumA += a * Kernel(ky)(kx)
        sumR += r * Kernel(ky)(kx)
        sumG += g * Kernel(ky)(kx)
        sumB += b * Kernel(ky)(kx)
      }

      mutableImg.set(x - MiddleIndexX, y - MiddleIndexY, getPixel(sumA / Divisor, sumR / Divisor, sumG / Divisor, sumB / Divisor))
    }

    mutableImg
  }
}

object Gaussian5Filter extends SlowKernelFilter {
  override val Kernel = Array(
    Array(1, 4, 7, 4, 1),
    Array(4, 16, 26, 16, 4),
    Array(7, 26, 41, 26, 7),
    Array(4, 16, 26, 16, 4),
    Array(1, 4, 7, 4, 1)
  )
  override val MiddleIndexX = 2
  override val MiddleIndexY = 2
  override val Divisor = 273
}

object LaplacianFilter extends SlowKernelFilter {
  override val Kernel = Array(
    Array(0, 1, 0),
    Array(1, -4, 1),
    Array(0, 1, 0)
  )

  override val Divisor = 1
  override val MiddleIndexX = 1
  override val MiddleIndexY = 1
}

object LaplacianOfGaussianFilter extends SlowKernelFilter {
  override val Kernel = Array(
    Array(0, 0, 3, 2, 2, 2, 3, 0, 0),
    Array(0, 2, 3, 5, 5, 5, 3, 2, 0),
    Array(3, 3, 5, 3, 0, 3, 5, 3, 3),
    Array(2, 5, 3, -12, -23, -12, 3, 5, 2),
    Array(2, 5, 0, -23, -40, -23, 0, 5, 2),
    Array(2, 5, 3, -12, -23, -12, 3, 5, 2),
    Array(3, 3, 5, 3, 0, 3, 5, 3, 3),
    Array(0, 2, 3, 5, 5, 5, 3, 2, 0),
    Array(0, 0, 3, 2, 2, 2, 3, 0, 0)
  )
  override val Divisor = 1
  override val MiddleIndexX = 4
  override val MiddleIndexY = 4
}

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
