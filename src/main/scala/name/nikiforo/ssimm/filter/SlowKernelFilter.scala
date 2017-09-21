package name.nikiforo.ssimm.filter

import name.nikiforo.ssimm._

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

object Gaussian5SlowKernelFilter extends SlowKernelFilter {
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

object LaplacianSlowKernelFilter extends SlowKernelFilter {
  override val Kernel = Array(
    Array(0, 1, 0),
    Array(1, -4, 1),
    Array(0, 1, 0)
  )

  override val Divisor = 1
  override val MiddleIndexX = 1
  override val MiddleIndexY = 1
}

object LaplacianOfGaussianSlowKernelFilter extends SlowKernelFilter {
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
