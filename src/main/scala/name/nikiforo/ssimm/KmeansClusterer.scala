package name.nikiforo.ssimm

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

case class Center(r: Float, g: Float, b: Float) {
  private val epsilon = 0.5

  val rInt = r.toInt
  val gInt = g.toInt
  val bInt = b.toInt

  def manhDist(r: Int, g: Int, b: Int): Float =
    math.abs(this.r - r) + math.abs(this.g - g) + math.abs(this.b - b)

  def isClose(other: Center) =
    math.abs(this.r - other.r) < epsilon &&
      math.abs(this.g - other.g) < epsilon &&
      math.abs(this.b - other.b) < epsilon
}

class Sum {
  var r, g, b, length = 0

  def update(r: Int, g: Int, b: Int): Unit = {
    this.r += r
    this.g += g
    this.b += b
    this.length += 1
  }

  def getCenter =
    if(length == 0) None
    else Some(Center(r.toFloat / length, g.toFloat / length, b.toFloat / length))
}

case class Cluster(center: Center, sum: Sum)

object KMeansClusterHelper {
  val ColorChannelAmount = 3

  private def random(clusterAmount: Int, points: Array[Int]): Seq[Center] = {
    val length = points.length
    val pixelLength = length / ColorChannelAmount

    val set = mutable.Set[Center]()
    while (set.size < clusterAmount){
      val ind = Random.nextInt(pixelLength) * ColorChannelAmount
      val r = points(ind + 1)
      val g = points(ind + 2)
      val b = points(ind + 3)
      set += Center(r, g, b)
    }
    set.to[Vector]
  }

  private def median(clusterAmount: Int, points: Array[Int]): Seq[Center] = {
    val length = points.length
    val pixelLength = length / ColorChannelAmount
    val lengthPerCluster = length / clusterAmount
    val pixelLengthPerCluster = lengthPerCluster / ColorChannelAmount

    var maxR = 0
    var minR = 255
    var maxG = 0
    var minG = 255
    var maxB = 0
    var minB = 255

    for { i <- 0 until pixelLength} {
      val r = points(i)
      val g = points(i + 1)
      val b = points(i + 2)

      if(r > maxR) maxR = r
      if(r < minR) minR = r
      if(g > maxG) maxG = g
      if(g < minG) minG = g
      if(b > maxB) maxB = b
      if(b < minB) minB = b
    }

    val rRange = maxR - minR
    val gRange = maxG - minG
    val bRange = maxB - minB

    val sortByColorInd =
      if(rRange > gRange)
        if(rRange > bRange) 0
        else 2
      else if(gRange > bRange) 1
      else 0

    val colored = for {
      i <- 0 until pixelLength
      ind = i * 3
    } yield i -> points(ind + sortByColorInd)

    val sorted = colored.sortBy(_._2)

    for {
      i <- 0 until clusterAmount
      ind = i * pixelLengthPerCluster
    } yield {
      val pixelInd = sorted(ind)._1
      val r = points(pixelInd)
      val g = points(pixelInd + 1)
      val b = points(pixelInd + 2)

      Center(r, g, b)
    }
  }

  def chooseInitialCenters(clusterAmount: Int, points: Array[Int]): Seq[Center] = median(clusterAmount, points)
}

object KmeansClusterer {
  import KMeansClusterHelper.ColorChannelAmount
  import KMeansClusterHelper.chooseInitialCenters

  def cluster[T](clusterAmount: Int, points: Array[Int]): Seq[Center] = {
    val length = points.length
    val pixelLength = length / ColorChannelAmount

    @tailrec
    def iterate(iteration: Int, centers: Seq[Center]): Seq[Center] = {
      val clusters = centers.map { c =>
        Cluster(c, new Sum())
      }

      for {
        i <- 0 until pixelLength
        ind = i * 3
      } {
        val r = points(ind)
        val g = points(ind + 1)
        val b = points(ind + 2)

        val closest = clusters.minBy { _.center.manhDist(r, g, b) }
        closest.sum.update(r, g, b)
      }

      val newCenters = clusters.map(cluster => cluster.sum.getCenter.getOrElse(cluster.center))

      val allNotMoved = centers.zip(newCenters).forall{ case (prev, curr) => prev.isClose(curr) }
      if(allNotMoved) newCenters
      else iterate(iteration + 1, newCenters)
    }

    val initialCenters = chooseInitialCenters(clusterAmount, points)
    iterate(0, initialCenters)
  }
}
