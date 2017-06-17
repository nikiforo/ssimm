package name.nikiforo.immortalcombat

import java.nio.file.Paths

/** Is used to see what is the result of developed filters */
object Main {
  def main(args: Array[String]): Unit = {
    val img = Image.from(Paths.get("src/main/resources/aGirl.jpg"))

    implicit val resultContainer = OneDimArray
    val noFilter = img.applyFilter(NoFilter)
    val lightness = img.applyFilter(GrayLightnessFilter)
    val average = img.applyFilter(GrayAverageFilter)
    val luminosity = img.applyFilter(GrayLuminosityFilter)
    val gaus5 = img.applyFilter(Gaussian5Filter)
    val log = img.applyFilter(GrayAverageFilter).applyFilter(LaplacianOfGaussianFilter)
    val lapl = img.applyFilter(GrayAverageFilter).applyFilter(LaplacianFilter)
    val red = img.applyFilter(RedComponentFilter)
    val green = img.applyFilter(GreenComponentFilter)
    val blue = img.applyFilter(BlueComponentFilter)

    val outputDir = "out"

    img.to(Paths.get(outputDir, "original.png"), PNG)
    noFilter.to(Paths.get(outputDir, "noFilter.png"), PNG)
    lightness.to(Paths.get(outputDir, "lightness.png"), PNG)
    average.to(Paths.get(outputDir, "average.png"), PNG)
    luminosity.to(Paths.get(outputDir, "luminosity.png"), PNG)
    gaus5.to(Paths.get(outputDir, "gaus5.png"), PNG)
    log.to(Paths.get(outputDir, "log.png"), PNG)
    lapl.to(Paths.get(outputDir, "lapl.png"), PNG)
    red.to(Paths.get(outputDir, "red.png"), PNG)
    green.to(Paths.get(outputDir, "green.png"), PNG)
    blue.to(Paths.get(outputDir, "blue.png"), PNG)
  }
}
