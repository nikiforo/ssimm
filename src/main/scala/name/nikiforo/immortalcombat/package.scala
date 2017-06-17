package name.nikiforo

import java.awt.image.BufferedImage

package object immortalcombat {
  type Pixel = Int
  type ColorComponent = Int

  def getPixel(alpha: ColorComponent, red: ColorComponent, green: ColorComponent, blue: ColorComponent) =
    ((alpha & 0xFF) << 24) | ((red & 0xFF) << 16) | ((green & 0xFF) << 8) | ((blue & 0xFF) << 0)

  implicit class PixelWithComponents(val pixel: Pixel) extends AnyVal {
    def argb = (alpha, red, green, blue)
    def alpha: ColorComponent = (pixel >> 24) & 0x000000FF
    def red: ColorComponent = (pixel >> 16) & 0x000000FF
    def green: ColorComponent = (pixel >> 8 ) & 0x000000FF
    def blue: ColorComponent = pixel & 0x000000FF
  }

  implicit class BufferedAwtToImage(val bi: BufferedImage) extends AnyVal {
    def toImg = new Image {
      def width = bi.getWidth
      def height = bi.getHeight
      def apply(x: Int, y: Int) = bi.getRGB(x, y) // TYPE_INT_ARGB
    }

    def toMutableImg = new MutableImage {
      def width = bi.getWidth
      def height = bi.getHeight
      def apply(x: Int, y: Int) = bi.getRGB(x, y) // TYPE_INT_ARGB
      def set(x: Int, y: Int, pixel: Pixel) = bi.setRGB(x, y, pixel)
    }
  }

  implicit class ArrayToImage(val array: Array[Pixel]) extends AnyVal {
    def toImg(w: Int) = {
      require(array.length % w == 0)

      val h = array.length / w
      new Image {
        def width = w
        def height = h
        def apply(x: Int, y: Int) = array(x * h + y)
      }
    }

    def toMutableImg(w: Int) = {
      require(array.length % w == 0)

      val h = array.length / w
      new MutableImage {
        def width = w
        def height = h
        def apply(x: Int, y: Int) = array(x * h + y)
        def set(x: Int, y: Int, pixel: Pixel) = array(x * h + y) = pixel
      }
    }
  }

  /** Outer array(or arrays) contains arrays, that corresponds to a columns */
  implicit class Array2ToImage(val array: Array[Array[Pixel]]) extends AnyVal {
    def toImg = new Image {
      def width = array.length
      def height = array(0).length
      def apply(x: Int, y: Int) = array(x)(y)
    }

    def toMutableImg = new MutableImage {
      def width = array.length
      def height = array(0).length
      def apply(x: Int, y: Int) = array(x)(y)
      def set(x: Int, y: Int, pixel: Pixel) = array(x)(y) = pixel
    }
  }
}
