package name.nikiforo.immortalcombat

sealed trait ResultContainer { def apply(width: Int, height: Int): MutableImage }

object OneDimArray extends ResultContainer {
  def apply(width: Int, height: Int): MutableImage = {
    val array = new Array[Pixel](width * height)
    array.toMutableImg(width)
  }
}

object TwoDimArray extends ResultContainer {
  def apply(width: Int, height: Int): MutableImage = {
    val array = new Array[Array[Pixel]](width)
    for(i <- 0 until width) array(i) = new Array[Pixel](height)
    array.toMutableImg
  }
}
