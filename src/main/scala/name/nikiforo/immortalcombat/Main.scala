package name.nikiforo.immortalcombat

import java.nio.file.Paths

object Main {
  def main(args: Array[String]): Unit = {
    val img = Image.from(Paths.get("src/main/resources/aGirl.jpg"))

    implicit val resultContainer = OneDimArray
    val noFilter = img.applyFilter(NoFilter)
    val lightness = img.applyFilter(GrayLightnessFilter)
    val average = img.applyFilter(GrayAverageFilter)
    val luminosity = img.applyFilter(GrayLuminosityFilter)

    val outputDir = "out"

    img.to(Paths.get(outputDir, "original.png"), PNG)
    noFilter.to(Paths.get(outputDir, "noFilter.png"), PNG)
    lightness.to(Paths.get(outputDir, "lightness.png"), PNG)
    average.to(Paths.get(outputDir, "average.png"), PNG)
    luminosity.to(Paths.get(outputDir, "luminosity.png"), PNG)
  }
}
