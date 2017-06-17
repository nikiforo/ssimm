package name.nikiforo.ssimm

/**
  * Data, produced by application of a filter should be stored somewhere.
  * Implicit mechanism is used to define the container structure, that will
  * be used to store the results. */
sealed trait ResultContainer { def apply(width: Int, height: Int): MutableImage }

/** If OneDimArray is implicitly defined in scope of filter application, then a result
  * of filter application will be structured as one dimensional array. */
object OneDimArray extends ResultContainer {
  def apply(width: Int, height: Int): MutableImage = {
    val array = new Array[Pixel](width * height)
    array.toMutableImg(width)
  }
}

/** If TwoDimArray is implicitly defined in scope of filter application, then a result
  * of filter application will be structured as two dimensional array. */
object TwoDimArray extends ResultContainer {
  def apply(width: Int, height: Int): MutableImage = {
    val array = new Array[Array[Pixel]](width)
    for(i <- 0 until width) array(i) = new Array[Pixel](height)
    array.toMutableImg
  }
}
