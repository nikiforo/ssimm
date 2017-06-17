package name.nikiforo.ssimm

import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}
import javax.imageio.ImageIO

/** The simplest image representation */
trait Image {
  def width: Int
  def height: Int
  def apply(x: Int, y: Int): Pixel
  def applyFilter(filter: Filter)(implicit resultContainer: ResultContainer) = filter.apply(this)

  def to(path: Path, format: OutputFormat): Unit = {
    val os = Files.newOutputStream(path)
    val bi = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    for {
      i <- 0 until width
      j <- 0 until height
    } bi.setRGB(i, j, apply(i, j))
    ImageIO.write(bi, format.name, os)
    os.close()
  }
}

object Image {
  def from(path: Path): Image = {
    val is = Files.newInputStream(path)
    val img = ImageIO.read(is).toImg
    is.close()
    img
  }

  def zerosMutable(width: Int, height: Int)(implicit resultContainer: ResultContainer): MutableImage = resultContainer(width, height)
}

trait MutableImage extends Image {
  def set(x: Int, y: Int, pixel: Pixel)
}