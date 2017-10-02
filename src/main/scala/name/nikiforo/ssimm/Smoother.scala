package name.nikiforo.ssimm

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class Region(val color: Pixel,
             val coords: ListBuffer[Coord] = ListBuffer.empty,
             val sortedNeighbors: mutable.ListBuffer[(Int, Region)] = ListBuffer.empty) {

  def addNeighbor(other: Region): Unit = {
    if(! sortedNeighbors.exists{ case(_, r) => r == other }) {
      val distance = color.manhattanDistance(other.color)
      val ind = sortedNeighbors.view.takeWhile { case (d, _) =>
        d <= distance
      }.length

      sortedNeighbors.insert(ind, (distance, other))
    }
  }

  def removeNeighbor(other: Region): Boolean = {
    val removeInd = sortedNeighbors.indexWhere{ case(_, r) => r eq other }
    if(removeInd != -1) {
      sortedNeighbors.remove(removeInd)
      true
    } else false
  }

  def addNeighborForBoth(other: Region): Unit = {
    this.addNeighbor(other)
    other.addNeighbor(this)
  }

  def mergeTo(to: Region): Unit = {
    to.coords ++= this.coords
    to.removeNeighbor(this)
    this.removeNeighbor(to)
    this.sortedNeighbors.foreach{ case(_, r) => to.addNeighbor(r) }
  }
}

object Smoother {
  private def mark(img: Image): mutable.Set[Region] = {
    val map = mutable.Map.empty[Coord, Region]
    val set = mutable.Set.empty[Region]

    def append(i: Int, j: Int, lb: Region) = {
      map += (i, j) -> lb
      set += lb
      lb.coords += (i -> j)
    }

    def merge(oldLB: Region, newLB: Region): Unit = {
      oldLB.mergeTo(newLB)
      map.foreach { case (key, lb) => if(lb eq oldLB) map(key) = newLB }
      set.foreach { lb => if(lb.removeNeighbor(oldLB)) lb.addNeighbor(newLB) }
      set -= oldLB
      set += newLB
    }

    /* mark top line */
    append(0, 0, new Region(img(0,0)))
    for { i <- 1 until img.width } {
      val leftPixel = img(i - 1, 0)
      val currPixel = img(i, 0)
      val leftLB = map(i - 1, 0)

      val regionForCurrPixel =
        if(leftPixel == currPixel) leftLB
        else {
          val r = new Region(img(i, 0))
          r.addNeighborForBoth(leftLB)

          r
        }
      append(i, 0, regionForCurrPixel)
    }

    /* mark left line */
    for { j <- 1 until img.height } {
      val topPixel = img(0, j - 1)
      val currPixel = img(0, j)
      val topLB = map(0, j - 1)

      val regionForCurrPixel =
        if(topPixel == currPixel) topLB
        else {
          val r = new Region(currPixel)
          r.addNeighborForBoth(topLB)

          r
        }
      append(0, j, regionForCurrPixel)
    }

    for {
      i <- 1 until img.width
      j <- 1 until img.height
    } {
      map.remove(i - 1, j - 1) //This line significantly reduces complexity

      val leftPixel = img(i - 1, j)
      val topPixel = img(i, j - 1)
      val curPixel = img(i, j)

      val leftLB = map(i - 1, j)
      val topLB = map(i, j - 1)

      val regionForCurrPixel = (leftPixel == curPixel, topPixel == curPixel) match {
        case (true, true) =>
          if(leftLB eq topLB) leftLB
          else {
            merge(leftLB, topLB)

            topLB
          }
        case (true, false) =>
          leftLB.addNeighborForBoth(topLB)

          leftLB
        case (false, true) =>
          leftLB.addNeighborForBoth(topLB)

          topLB
        case (false, false) =>
          val r = new Region(img(i,j))
          r.addNeighborForBoth(leftLB)
          r.addNeighborForBoth(topLB)

          r
      }

      append(i, j, regionForCurrPixel)
    }

    assert(set == set.flatMap(_.sortedNeighbors.map(_._2)), "not equal")
    set.foreach{ r => assert(! r.sortedNeighbors.exists{ case (_, sortedRegion) => sortedRegion eq r }) }

    set
  }

  def getRegions(img: Image): Seq[Region] = {
    val start = System.currentTimeMillis()
    val result = mark(img).toSeq
    val end = System.currentTimeMillis()
    println(s"took ${end-start} msecs")
    result
  }

  def smooth(img: Image, colorDiffThreshold: Int, regionSizeThreshold: Int)(implicit resultContainer: ResultContainer): Image = {
    val regs = getRegions(img)

    val sortedRegions = regs.map(r => r.coords.size -> r).sortBy(_._1).to[ListBuffer]

    def go(currInd: Int, regions: ListBuffer[(Int, Region)]): ListBuffer[(Int, Region)] = {
      val regionsLength = regions.length
      println(currInd)
      println(regionsLength)
      if(currInd == regionsLength - 1) regions
      else {
        val firstRegionToMerge = regions(currInd)._2
        if(firstRegionToMerge.coords.size > regionSizeThreshold || firstRegionToMerge.sortedNeighbors.isEmpty) regions
        else if(firstRegionToMerge.sortedNeighbors.head._1 > colorDiffThreshold) go(currInd + 1, regions)
        else {
          val secondRegionToMerge = firstRegionToMerge.sortedNeighbors.head._2
          firstRegionToMerge.mergeTo(secondRegionToMerge)
          regions.remove(currInd)
          regions.remove(regions.indexWhere{ case (_, r) => r eq secondRegionToMerge })
          regions.foreach { case (_, r) =>
            val reg1RemovalResult = r.removeNeighbor(firstRegionToMerge)
            val reg2RemovalResult = r.removeNeighbor(secondRegionToMerge)
            if(r != secondRegionToMerge && (reg1RemovalResult || reg2RemovalResult)) r.addNeighbor(secondRegionToMerge)
          }
          val newRegionLength = secondRegionToMerge.coords.size
          val indToInsert = regions.view.takeWhile{ case (length, _) => length <= newRegionLength }.length
          regions.insert(indToInsert, newRegionLength -> secondRegionToMerge)
          go(currInd, regions)
        }
      }
    }

    val resultImg = resultContainer(img.width, img.height)
    for {
      (_, region) <- go(0, sortedRegions)
      (i, j) <- region.coords
    } {
      resultImg.set(i, j, region.color)
    }

    resultImg
  }
}
