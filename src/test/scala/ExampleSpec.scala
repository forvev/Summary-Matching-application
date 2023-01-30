import org.scalatest.flatspec.AnyFlatSpec
import play.api.libs.json._
import java.nio.file._


class ExampleSpec extends AnyFlatSpec {
  //With this input I expect this output...
  val xml_urls_path = "./src/main/xml-files"
  val projectJAR2 = "./src/main/jar-files/STD_app_more_types.jar"
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







}