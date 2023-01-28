import org.scalatest.flatspec.AnyFlatSpec

import collection.mutable
import org.scalatest._
import play.api.libs.json._

import java.nio.file._
import java.io.{File, PrintWriter}
//import scala.reflect.io.File
import scala.util.parsing.json.JSONObject

class ExampleSpec extends AnyFlatSpec {
  //With this input I expect this output...
  val xml_urls_path = "./src/main/xml-files"
  val projectJAR2 = "./src/main/jar-files/STD_app.jar"
  val object_to_test = new SearchForDependencies(xml_urls_path = xml_urls_path, jar_path = projectJAR2)
  object_to_test.execute
  "Class with dependencies" can "have size more than 0" in {
    assert(object_to_test.classWithDependencies.empty != 0)
  }

  //assert(Test.projectJAR2 == "./src/main/jar-files/STD_app2.jar")

  "class with match summary" can "have size more than 0" in {
    assert(object_to_test.classWithMatchSummary.empty != 0)
  }

  "inside classWithMatchSummary" must "class ... must be included" in {

    print("list of match summaries: " + object_to_test.classWithMatchSummary)
    print("\nlist of dependencies: " + object_to_test.classWithDependencies)
    //we check if the match summary exists
    val containsName = object_to_test.classWithMatchSummary.contains("com.example.std_app.TestClassSummary")
    assert(containsName == true, "Test_ClassSummary doesn't exist!")
  }

  //If the file exists already, delete it
  if (Files.exists(Paths.get("./src/main/JSON/match_summaries.json"))){
    Files.deleteIfExists(Paths.get("./src/main/JSON/match_summaries.json"))
  }

  //create a new JSON file with the summaries
  val path = Paths.get("./src/main/JSON/match_summaries.json")
  Files.createFile(path)
  object_to_test.classWithMatchSummary.foreach(u => {
    val data = Json.obj("Class_name" -> u._1, "Summary_name" -> u._2.summary_Name)
    val json = data.toString()
    Files.write(Paths.get("./src/main/JSON/match_summaries.json"), (json+"\n").getBytes(), StandardOpenOption.APPEND)
  })

  //dependencies part
  if (Files.exists(Paths.get("./src/main/JSON/match_dependencies.json"))) {
    Files.deleteIfExists(Paths.get("./src/main/JSON/match_dependencies.json"))
  }

  //create a new JSON file with the dependencies
  val path_2 = Paths.get("./src/main/JSON/match_dependencies.json")
  Files.createFile(path_2)
  object_to_test.classWithDependencies.foreach(u => {
    val data = Json.obj("Class_name" -> u._1, "dependent_class/es" -> u._2)
    val json = data.toString()
    Files.write(Paths.get("./src/main/JSON/match_dependencies.json"), (json + "\n").getBytes(), StandardOpenOption.APPEND)
  })


}