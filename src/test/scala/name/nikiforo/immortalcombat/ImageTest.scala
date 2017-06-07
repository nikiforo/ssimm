package name.nikiforo.immortalcombat

import java.nio.file.Paths

import com.google.common.jimfs.{Configuration, Jimfs}
import org.scalatest.FunSuite

class ImageTest extends FunSuite {
  val alpha = 0xff
  test("Pixel") {
    val pixel = getPixel(alpha, 10, 20, 30)
    assert(pixel.red == 10)
    assert(pixel.green == 20)
    assert(pixel.blue == 30)
  }

  test("Pixel2") {
    val pixel = 0x00ffeecc
    assert(pixel.red == 0xff)
    assert(pixel.green == 0xee)
    assert(pixel.blue == 0xcc)
  }

  test("getPixel cares only about last byte") {
    assert(getPixel(alpha, 0xffffffff, 0xffffffee, 0xffffffcc) == 0xffffeecc)
  }

  test("Load/save for image works") {
    val url = this.getClass.getClassLoader.getResource("aGirl.jpg")
    val img = Image.from(Paths.get(url.toURI))

    val fs = Jimfs.newFileSystem(Configuration.unix())
    val path = fs.getPath("/aGirl.png")
    img.to(path, PNG)

    val img2 = Image.from(path)
    for {
      i <- 0 until img.width
      j <- 0 until img.height
    } assert(img(i, j) == img2(i, j))
  }

  test("NoFilter does nothing with an image for all containers"){
    val url = this.getClass.getClassLoader.getResource("aGirl.jpg")
    val img = Image.from(Paths.get(url.toURI))

    val noFilterOneDim = img.applyFilter(NoFilter)(OneDimArray)
    val noFilterTwoDim = img.applyFilter(NoFilter)(TwoDimArray)

    for {
      x <- 0 until img.width
      y <- 0 until img.height
    } {
      assert(noFilterOneDim(x, y) == img(x, y), s"failed for (i: $x, j: $y)")
      assert(noFilterTwoDim(x, y) == img(x, y))
    }
  }

  test("Gray average filter works") {
    val img = Array(getPixel(alpha, 0, 10, 20), getPixel(alpha, 40, 20, 30), getPixel(alpha, 20, 30, 10), getPixel(alpha, 50, 40, 30)).toImg(2)

    implicit val resultContainer = OneDimArray
    val grayed = img.applyFilter(GrayAverageFilter)

    assert(grayed(0, 0) == getPixel(alpha, 10,10,10))
    assert(grayed(0, 1) == getPixel(alpha, 30,30,30))
    assert(grayed(1, 0) == getPixel(alpha, 20,20,20))
    assert(grayed(1, 1) == getPixel(alpha, 40,40,40))
  }
}
