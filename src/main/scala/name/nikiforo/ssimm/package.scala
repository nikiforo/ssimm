package name.nikiforo

import java.awt.image.BufferedImage

package object ssimm {
  type Pixel = Int
  type ColorComponent = Int
  type Coord = (Int, Int)

  val MaxColorValue = 0xff
  val MinColorValue = 0x00
  val Black = getPixel(MinColorValue, MinColorValue, MinColorValue)
  val White = getPixel(MaxColorValue, MaxColorValue, MaxColorValue)
  val Red = getPixel(MaxColorValue, MinColorValue, MinColorValue)
  val Green = getPixel(MinColorValue, MaxColorValue, MinColorValue)
  val Blue = getPixel(MinColorValue, MinColorValue, MaxColorValue)

  def getPixel(alpha: ColorComponent, red: ColorComponent, green: ColorComponent, blue: ColorComponent): Pixel =
    ((alpha & 0xFF) << 24) | ((red & 0xFF) << 16) | ((green & 0xFF) << 8) | ((blue & 0xFF) << 0)

  def getPixel(red: ColorComponent, green: ColorComponent, blue: ColorComponent): Pixel =
    getPixel(0xff, red, green, blue)

  implicit class PixelWithComponents(val pixel: Pixel) extends AnyVal {
    def argb = (alpha, red, green, blue)
    def alpha: ColorComponent = (pixel >> 24) & 0x000000FF
    def red: ColorComponent = (pixel >> 16) & 0x000000FF
    def green: ColorComponent = (pixel >> 8 ) & 0x000000FF
    def blue: ColorComponent = pixel & 0x000000FF
    def manhattanDistance(other: Pixel) = {
      math.abs(alpha - other.alpha) + math.abs(red - other.red) +
        math.abs(green - other.green) + math.abs(blue - other.blue)
    }
  }

  implicit class BufferedAwtToImage(val bi: BufferedImage) extends AnyVal {
    def toImg = new Image {
      val width = bi.getWidth
      val height = bi.getHeight
      def apply(x: Int, y: Int) = bi.getRGB(x, y) // TYPE_INT_ARGB
    }

    def toMutableImg = new MutableImage {
      val width = bi.getWidth
      val height = bi.getHeight
      def apply(x: Int, y: Int) = bi.getRGB(x, y) // TYPE_INT_ARGB
      def set(x: Int, y: Int, pixel: Pixel) = bi.setRGB(x, y, pixel)
    }
  }

  implicit class ArrayToImage(val array: Array[Pixel]) extends AnyVal {
    def toImg(w: Int): Image = {
      require(array.length % w == 0)

      val h = array.length / w
      new Image {
        val width = w
        val height = h
        def apply(x: Int, y: Int) = array(x * h + y)
      }
    }

    def toMutableImg(w: Int): MutableImage = {
      require(array.length % w == 0)

      val h = array.length / w
      new MutableImage {
        val width = w
        val height = h
        def apply(x: Int, y: Int) = array(x * h + y)
        def set(x: Int, y: Int, pixel: Pixel) = array(x * h + y) = pixel
      }
    }
  }

  /** Outer array(or arrays) contains arrays, that corresponds to a columns */
  implicit class Array2ToImage(val array: Array[Array[Pixel]]) extends AnyVal {
    def toImg = new Image {
      val width = array.length
      val height = array(0).length
      def apply(x: Int, y: Int) = array(x)(y)
    }

    def toMutableImg = new MutableImage {
      val width = array.length
      val height = array(0).length
      def apply(x: Int, y: Int) = array(x)(y)
      def set(x: Int, y: Int, pixel: Pixel) = array(x)(y) = pixel
    }
  }
}
