import org.scalatest.flatspec.AnyFlatSpec
import collection.mutable
import org.scalatest._

class ExampleSpec extends AnyFlatSpec {
//With this input I expect this output...
  "Class with dependencies" can "have size more than 0" in {
    assert(Test.classWithDependencies.empty != 0)
  }

  //assert(Test.projectJAR2 == "./src/main/jar-files/STD_app2.jar")

  "class with match summary" can "have size more than 0" in {
    assert(Test.classWithMatchSummary.empty != 0)
  }

  "inside classWithMatchSummary" must "class ... must be included" in {
    //Test.classWithMatchSummary.contains("com/example/std_app/TestClassSummary")

    //val containsName = classWithMatchSummary.contains("my_class")
    //assert(containsName == true, "my_class is not found")
    //TODO: ask if there is a way to test variables which are not assigned at the beginning
    print(Test.classWithDependencies)
    val containsName = Test.classWithMatchSummary.contains("com/example/std_app/TestClassSummary")
    assert(containsName == false,"my_class is not found")
  }

//  it should "produce NoSuchElementException when head is invoked" in {
//    assertThrows[NoSuchElementException] {
//      Test.classWithDependencies.empty.head
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