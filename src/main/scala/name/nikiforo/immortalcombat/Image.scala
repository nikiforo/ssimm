package name.nikiforo.immortalcombat

import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}
import javax.imageio.ImageIO

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
}

trait MutableImage extends Image {
  def set(x: Int, y: Int, pixel: Pixel)
}

sealed trait OutputFormat { def name: String }
object JPEG extends OutputFormat { val name = "jpeg" }
object PNG extends OutputFormat { val name = "png" }
object BMP extends OutputFormat { val name = "bmp" }
object WBMP extends OutputFormat { val name = "wbmp"}
object GIF extends OutputFormat { val name = "gif" }