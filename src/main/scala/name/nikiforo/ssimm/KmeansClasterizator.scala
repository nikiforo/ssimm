package name.nikiforo.ssimm

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Random

object KmeansClasterizator
{
  type Point = (Int, Int, Int, Int)
  type Center = (Double, Double, Double, Double)
  type Sum = (Long, Long, Long, Long)

  private val epsilon = 0.5
  private def isClose(c1: Center, c2: Center) = {
    val (a1, r1, g1, b1) = c1
    val (a2, r2, g2, b2) = c2

    math.abs(a1 - a2) < epsilon &&
      math.abs(r1 - r2) < epsilon &&
      math.abs(g1 - g2) < epsilon &&
      math.abs(b1 - b2) < epsilon
  }

  private def distance(c: Center, p: Point) = {
    val (aCenter, rCenter, gCenter, bCenter) = c
    val (aPoint, rPoint, gPoint, bPoint) = p
    math.abs(aCenter - aPoint) + math.abs(rCenter - rPoint) + math.abs(gCenter - gPoint) + math.abs(bCenter - bPoint)
  }

  private def divide(sum: Sum, divider: Int) = {
    val (a, r, g, b) = sum
    (a.toDouble / divider, r.toDouble / divider, g.toDouble / divider, b.toDouble / divider)
  }

  private def toDouble(point: Point) = {
    val (aPoint, rPoint, gPoint, bPoint) = point
    (aPoint.toDouble, rPoint.toDouble, gPoint.toDouble, bPoint.toDouble)
  }

  val EmptySum = (0L, 0L, 0L, 0L)

  case class Cluster(center: Center, sum: Sum, points: List[Point])

  def clasterize[T](clusterAmount: Int, points: List[Point]) = {
    @tailrec
    def iterate(prevClusters: IndexedSeq[Cluster]): IndexedSeq[Cluster] = {
      val emptyClusters = prevClusters.map { c =>
        Cluster(c.center, EmptySum, List.empty)
      }

      var preFilledClusters = emptyClusters
      points.foreach { p =>
        val closest = preFilledClusters.minBy { c => distance(c.center, p) }
        val (aCluster, rCluster, gCluster, bCluster) = closest.sum
        val (aPoint, rPoint, gPoint, bPoint) = p
        val newSum = (aCluster + aPoint, rCluster + rPoint, gCluster + gPoint, bCluster + bPoint)
        val newPoints = p +: closest.points

        val ind = preFilledClusters.indexOf(closest)
        preFilledClusters = preFilledClusters.updated(ind, Cluster(closest.center, newSum, newPoints))
      }

      val filledClusters = preFilledClusters.map(c => c.copy(center = divide(c.sum, c.points.length)))

      val allNotMoved = prevClusters.zip(filledClusters).map { case (prev, curr) => isClose(prev.center, curr.center) }.reduce(_ && _)
      if (allNotMoved) filledClusters
      else iterate(filledClusters)
    }

    val length = points.length

    val initialCentroids = {
      var set = mutable.Set[Point]()
      while (set.size < clusterAmount) set += points(Random.nextInt(length))
      set.map(p => Cluster(toDouble(p), EmptySum, List.empty)).to[Vector]
    }

    iterate(initialCentroids)
  }
}
