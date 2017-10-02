package name.nikiforo.ssimm

import java.nio.file.Paths

import com.google.common.jimfs.{Configuration, Jimfs}
import name.nikiforo.ssimm.filter._
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

  test("NoFilter does nothing with an image for all containers") {
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
    implicit val resultContainer = OneDimArray

    val img: Image = {
      val i = Image.zerosMutable(2, 2)
      i.set(0, 0, getPixel(alpha, 0, 10, 20))
      i.set(0, 1, getPixel(alpha, 40, 20, 30))
      i.set(1, 0, getPixel(alpha, 20, 30, 10))
      i.set(1, 1, getPixel(alpha, 50, 40, 30))

      i
    }

    val grayed = img.applyFilter(GrayAverageFilter)

    assert(grayed(0, 0) == getPixel(alpha, 10, 10, 10))
    assert(grayed(0, 1) == getPixel(alpha, 30, 30, 30))
    assert(grayed(1, 0) == getPixel(alpha, 20, 20, 20))
    assert(grayed(1, 1) == getPixel(alpha, 40, 40, 40))
  }

  test("slow gaussian 5") {
    implicit val resultContainer = OneDimArray
    val everyComponentArray = Array(
      Array(0, 1, 2, 3, 4),
      Array(5, 6, 7, 8, 9),
      Array(0, 1, 2, 3, 4),
      Array(5, 6, 7, 8, 9),
      Array(0, 1, 2, 3, 4)
    )

    val img: Image = {
      val i = Image.zerosMutable(5, 5)

      for {
        x <- 0 until 5
        y <- 0 until 5
      } {
        val channel = everyComponentArray(y)(x) * 10 //multiply by 10 to get more accurate result in assert
        i.set(x, y, getPixel(alpha, channel, channel, channel))
      }

      i
    }

    val withGauss = img.applyFilter(Gaussian5SlowKernelFilter)
    assert(withGauss.width == 1)
    assert(withGauss.height == 1)
    val res = 44 //I've computed it using calculator
    assert(withGauss(0, 0) == getPixel(alpha, res, res, res))
  }

  test("KMeansClusterer choose initial clusters") {
    implicit val resultContainer = OneDimArray
    def bluePixel(blue: Int) = getPixel(alpha, 0, 0, blue)

    val seq = Array(
      bluePixel(1),
      bluePixel(2),
      bluePixel(3),
      bluePixel(4),
      bluePixel(5),
      bluePixel(6),
      bluePixel(7)
    )

    val img: Image = seq.toImg(1)

    val kMeansFormattedImage = KMeansFilterHelper.convertToKMeansFormat(img)
    val centers = KmeansClusterer.cluster(1, kMeansFormattedImage)

    assert(centers.length == 1 && centers.head.isClose(Center(0, 0, 4)))
  }

  test("KMeansClusterer single pixel") {
    implicit val resultContainer = OneDimArray
    val originalPixel = getPixel(alpha, 10, 20, 30)

    val img: Image = {
      val i = Image.zerosMutable(1, 1)
      i.set(0, 0, originalPixel)
      i
    }

    val clustered = img.applyFilter(new KMeansFilter(clusterAmount = 1))

    assert(clustered(0, 0) == originalPixel)
  }

  test("KMeansClusterer single cluster") {
    implicit val resultContainer = OneDimArray
    val originalPixel1 = getPixel(alpha, 10, 20, 30)
    val originalPixel2 = getPixel(alpha, 30, 20, 10)

    val img: Image = {
      val i = Image.zerosMutable(2, 2)
      i.set(0, 0, originalPixel1)
      i.set(0, 1, originalPixel1)
      i.set(1, 0, originalPixel2)
      i.set(1, 1, originalPixel2)
      i
    }

    val clustered = img.applyFilter(new KMeansFilter(clusterAmount = 1))

    assert(clustered(0, 0) == getPixel(alpha, 20, 20, 20))
  }


  test("Smoother two regions") {
    implicit val resultContainer = OneDimArray
    val originalPixel1 = getPixel(alpha, 10, 20, 30)
    val originalPixel2 = getPixel(alpha, 30, 20, 10)

    val img: Image = {
      val i = Image.zerosMutable(2, 2)
      i.set(0, 0, originalPixel1)
      i.set(0, 1, originalPixel1)
      i.set(1, 0, originalPixel2)
      i.set(1, 1, originalPixel2)
      i
    }

    assert(Smoother.getRegions(img).length == 2)
  }

  test("Smoother four regions") {
    implicit val resultContainer = OneDimArray
    val originalPixel1 = getPixel(alpha, 10, 20, 30)
    val originalPixel2 = getPixel(alpha, 30, 20, 10)

    val img: Image = {
      val i = Image.zerosMutable(2, 2)
      i.set(0, 0, originalPixel1)
      i.set(0, 1, originalPixel2)
      i.set(1, 0, originalPixel2)
      i.set(1, 1, originalPixel1)
      i
    }

    assert(Smoother.getRegions(img).length == 4)
  }

  test("Smoother one region") {
    implicit val resultContainer = OneDimArray
    val originalPixel1 = getPixel(alpha, 10, 20, 30)

    val img: Image = {
      val i = Image.zerosMutable(2, 2)
      i.set(0, 0, originalPixel1)
      i.set(0, 1, originalPixel1)
      i.set(1, 0, originalPixel1)
      i.set(1, 1, originalPixel1)
      i
    }

    assert(Smoother.getRegions(img).length == 1)
  }

  test("Region") {
    val originalPixel1 = getPixel(alpha, 10, 20, 30)
    val originalPixel2 = getPixel(alpha, 30, 20, 10)
    val originalPixel3 = getPixel(alpha, 10, 20, 31)

    val r1 = new Region(originalPixel1)
    val r2 = new Region(originalPixel2)
    val r3 = new Region(originalPixel3)

    /* addition */
    r1.addNeighbor(r2)
    assert(r1.sortedNeighbors.length == 1)
    assert(r2.sortedNeighbors.isEmpty)

    /* add both */
    r1.addNeighborForBoth(r2)
    assert(r1.sortedNeighbors.length == 1)
    assert(r2.sortedNeighbors.length == 1)

    /* add both with another*/
    r1.addNeighborForBoth(r3)
    assert(r1.sortedNeighbors.length == 2)
    assert(r3.sortedNeighbors.length == 1)

    /* closer regions in the begining */
    assert(r1.sortedNeighbors.head._2 == r3)
    assert(r1.sortedNeighbors(1)._2 == r2)
  }
}