abstract class Exercise extends App {
  val day: String = getClass.getSimpleName.replaceAll("\\$", "")
  val inputFileName: String = day + "_input.txt"
  val path: String = "data/" + inputFileName
}
