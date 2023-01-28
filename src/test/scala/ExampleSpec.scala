import org.scalatest.flatspec.AnyFlatSpec
import collection.mutable
import org.scalatest._

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
    //Test.classWithMatchSummary.contains("com/example/std_app/TestClassSummary")

    //val containsName = classWithMatchSummary.contains("my_class")
    //assert(containsName == true, "my_class is not found")
    //TODO: ask if there is a way to test variables which are not assigned at the beginning
    print("list of match summaries: "+object_to_test.classWithMatchSummary)
    print("\nlist of dependencies: "+object_to_test.classWithDependencies)
    //we check if the match summary exists
    val containsName = object_to_test.classWithMatchSummary.contains("com.example.std_app.TestClassSummary")
    assert(containsName == true,"Test_ClassSummary doesn't exist!")

//    //check if the dependency exists
//    val containsName_depend_1 = object_to_test.classWithDependencies.contains("com.example.std_app.TestClassSummary")
//    val containsName_depend_2 = object_to_test.classWithDependencies.contains("java.lang.System")
//
//    assert(containsName_depend_1 == containsName_depend_2, "there is no dependency!")
  }

//  it should "produce NoSuchElementException when head is invoked" in {
//    assertThrows[NoSuchElementException] {
//      object_to_test.classWithDependencies.empty.head
//    }
//  }

  "A mutable Set" should "allow an element to be added" in {

    info("info is recorded")
    markup("markup is *also* recorded")
    note("notes are sent immediately")
    alert("alerts are also sent immediately")

    val set = mutable.Set.empty[String]
    set += "clarity"
    assert(set.size === 1)
    assert(set.contains("clarity"))
  }
}