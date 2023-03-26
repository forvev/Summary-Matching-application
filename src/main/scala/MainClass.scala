import java.nio.file.Paths

/**
 *
 * @Authors: Artur, Tam
 */

object MainClass {
  def main(args: Array[String]): Unit = {

    if (args.length < 6){
      println("Please set parameters:")
      println("-i <path to jar file of Android application>")
      println("-s <path to summaries>")
      println("-o <path to output>")
      return
    }

    val isInputValid = args(0) == "-i" && args(2) == "-s" && args(4) == "-o"
    if (!isInputValid)
      return

    val jar_path = args(1)
    val summaries_path = args(3)
    val output_path = args(5)
    val object_to_test = new SearchForDependencies(jar_path = jar_path, summaries_path = summaries_path, output_path = output_path)
    object_to_test.execute
  }


}
