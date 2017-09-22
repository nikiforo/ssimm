package name.nikiforo.ssimm

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class Center(a: Float, r: Float, g: Float, b: Float) {
  private val epsilon = 0.5

  def manhDist(a: Int, r: Int, g: Int, b: Int): Float =
    math.abs(this.a - a) + math.abs(this.r - r) + math.abs(this.g - g) + math.abs(this.b - b)

  def isClose(other: Center) =
    math.abs(this.a - other.a) < epsilon &&
      math.abs(this.r - other.r) < epsilon &&
      math.abs(this.g - other.g) < epsilon &&
      math.abs(this.b - other.b) < epsilon
}

class Sum(var a: Int, var r: Int, var g: Int, var b: Int) {
  def update(a: Int, r: Int, g: Int, b: Int): Unit = {
    this.a += a
    this.r += r
    this.g += g
    this.b += b
  }

  def getCenter(length: Int) = Center(this.a.toFloat / length, this.r.toFloat / length, this.g.toFloat / length, this.b.toFloat / length)
}

case class Cluster(center: Center, sum: Sum, points: scala.collection.mutable.ArrayBuffer[Int])

object KmeansClasterizator
{
  def emptySum = new Sum(0, 0, 0, 0)

  def clasterize[T](clusterAmount: Int, points: Array[Int]) = {
    val length = points.length
    val pixelLength = length / 4
    val lengthPerCluster = length / clusterAmount

    @tailrec
    def iterate(centers: IndexedSeq[Center]): IndexedSeq[Cluster] = {
      val clusters = centers.map { c =>
        Cluster(c, emptySum, new ArrayBuffer[Int](lengthPerCluster))
      }

      for {
        i <- 0 until pixelLength
        ind = i * 4
      } {
        val a = points(ind)
        val r = points(ind + 1)
        val g = points(ind + 2)
        val b = points(ind + 3)

        val closest = clusters.minBy { _.center.manhDist(a, r, g, b) }
        closest.sum.update(a, r, g, b)
        closest.points += a
        closest.points += r
        closest.points += g
        closest.points += b
      }

      val newCenters = clusters.map { cluster =>
        cluster.sum.getCenter(cluster.points.length / 4)
      }

      val allNotMoved = centers.zip(newCenters).forall{ case (prev, curr) => prev.isClose(curr) }
      if(allNotMoved) clusters.zip(newCenters).map { case(cluster, cntr) => cluster.copy(center = cntr) }
      else iterate(newCenters)
    }

    /* Here can be some better heuristic to find initial centers */
    val initialCenters = {
      val set = mutable.Set[Center]()
      while (set.size < clusterAmount){
        val ind = Random.nextInt(pixelLength) * 4
        val a = points(ind)
        val r = points(ind + 1)
        val g = points(ind + 2)
        val b = points(ind + 3)
        set += Center(a, r, g, b)
      }
      set.to[Vector]
    }

    iterate(initialCenters)
  }
}
