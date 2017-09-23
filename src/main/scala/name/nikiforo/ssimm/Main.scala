package name.nikiforo.ssimm

import java.nio.file.Paths

import name.nikiforo.ssimm.filter._

/** Is used to see what is the result of developed filters */
object Main {
  def main(args: Array[String]): Unit = {
    val img = Image.from(Paths.get("src/main/resources/lop2.jpg"))

    implicit val resultContainer = OneDimArray
    val noFilter = img.applyFilter(NoFilter)
    val lightness = img.applyFilter(GrayLightnessFilter)
    val average = img.applyFilter(GrayAverageFilter)
    val luminosity = img.applyFilter(GrayLuminosityFilter)
    val gaus5 = img.applyFilter(Gaussian5SlowKernelFilter)
    val log = img.applyFilter(GrayAverageFilter).applyFilter(LaplacianOfGaussianSlowKernelFilter)
    val lapl = img.applyFilter(GrayAverageFilter).applyFilter(LaplacianSlowKernelFilter)
    val red = img.applyFilter(RedComponentFilter)
    val green = img.applyFilter(GreenComponentFilter)
    val blue = img.applyFilter(BlueComponentFilter)
    val clustered = img.applyFilter(new KMeansFilter(clusterAmount = 25))

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
    clustered.to(Paths.get(outputDir, "clustered.png"), PNG)
  }
}
