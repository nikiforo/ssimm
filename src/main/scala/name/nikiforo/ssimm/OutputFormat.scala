package name.nikiforo.ssimm

sealed trait OutputFormat { def name: String }
object JPEG extends OutputFormat { val name = "jpeg" }
object PNG extends OutputFormat { val name = "png" }
object BMP extends OutputFormat { val name = "bmp" }
object WBMP extends OutputFormat { val name = "wbmp"}
object GIF extends OutputFormat { val name = "gif" }