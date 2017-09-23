package name.nikiforo.ssimm

import java.nio.file.{Path, Paths}
import name.nikiforo.ssimm.filter.{Gaussian5SlowKernelFilter, GrayAverageFilter, KMeansFilter, NoFilter}


case class PerfomanceTest(timeMillis: Long,
                          image: String,
                          noFilter: Option[Long],
                          grayAverageFilter: Option[Long],
                          gaussian5SlowKernelFilter: Option[Long],
                          kMeansFilter: Option[Long])

object PerfomanceTest {
  def now = System.currentTimeMillis()
  private def getMsec(f: () => Unit): Long = {
    val start = now
    f()
    val end = now
    end - start
  }

  def measure(path: Path): PerfomanceTest = {
    val img = Image.from(path)
    implicit val resultContainer = OneDimArray

    val noFilter = getMsec { () => img.applyFilter(NoFilter) }
    val grayAverageFilter = getMsec { () => img.applyFilter(GrayAverageFilter) }
    val gaussian5SlowKernelFilter = getMsec { () => img.applyFilter(Gaussian5SlowKernelFilter) }
    val kMeansFilter = getMsec { () => img.applyFilter(new KMeansFilter(clusterAmount = 25)) }

    PerfomanceTest(
      timeMillis = now,
      image = path.getFileName.toString,
      noFilter = Some(noFilter),
      grayAverageFilter = Some(grayAverageFilter),
      gaussian5SlowKernelFilter = Some(gaussian5SlowKernelFilter),
      kMeansFilter = Some(kMeansFilter)
    )
  }

  def test(): PerfomanceTest = {
    val path = Paths.get("src/main/resources/lop2.jpg")
    val tests = for(i <- 1 to 10) yield {
      println(s"measuring $i")
      measure(path)
    }

    PerfomanceTest(
      timeMillis = now,
      image = path.getFileName.toString,
      noFilter = Some((tests.flatMap(_.noFilter).sum.toDouble / tests.length).toInt),
      grayAverageFilter = Some((tests.flatMap(_.grayAverageFilter).sum.toDouble / tests.length).toInt),
      gaussian5SlowKernelFilter = Some((tests.flatMap(_.gaussian5SlowKernelFilter).sum.toDouble / tests.length).toInt),
      kMeansFilter = Some((tests.flatMap(_.kMeansFilter).sum.toDouble / tests.length).toInt)
    )
  }
}
