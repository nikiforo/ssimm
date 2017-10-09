package name.nikiforo.ssimm.filter

import name.nikiforo.ssimm.{Image, ResultContainer}

object TransposeFilter extends Filter {
  def apply(image: Image)(implicit resultContainer: ResultContainer): Image = {
    val result = resultContainer.apply(image.height, image.width)
    for {
      i <- 0 until image.width
      j <- 0 until image.height
    } result.set(j, i, image(i, j))
    result
  }
}

object RotateClockwiseFilter extends Filter {
  def apply(image: Image)(implicit resultContainer: ResultContainer): Image = {
    val result = resultContainer.apply(image.height, image.width)
    for {
      i <- 0 until image.width
      j <- 0 until image.height
    } result.set(result.width - 1 - j, i, image(i, j))
    result
  }
}

object RotateCounterClockwiseFilter extends Filter {
  def apply(image: Image)(implicit resultContainer: ResultContainer): Image = {
    val result = resultContainer.apply(image.height, image.width)
    for {
      i <- 0 until image.width
      j <- 0 until image.height
    } result.set(j, result.height - 1 - i, image(i, j))
    result
  }
}

object MirrorX extends Filter {
  def apply(image: Image)(implicit resultContainer: ResultContainer): Image = {
    val result = resultContainer.apply(image.width, image.height)
    for {
      i <- 0 until image.width
      j <- 0 until image.height
    } result.set(i, result.height - 1 - j, image(i, j))
    result
  }
}

object MirrorY extends Filter {
  def apply(image: Image)(implicit resultContainer: ResultContainer): Image = {
    val result = resultContainer.apply(image.width, image.height)
    for {
      i <- 0 until image.width
      j <- 0 until image.height
    } result.set(result.width - 1 - i, j, image(i, j))
    result
  }
}