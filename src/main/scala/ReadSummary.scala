import scala.collection.mutable.ListBuffer
import scala.xml.XML

class ReadSummary(var xml_url: String) {

  val xml = XML.loadFile(xml_url)
  val temp = (xml \\ "summary" \\ "methods " \ "method")

  def getMethods(): List[MethodSummary] = {
    var methods = new ListBuffer[MethodSummary]()
    (xml \\ "summary" \\ "methods" \\ "method" \\ "@id").foreach(method => {
      println(method.toString())
    })
    methods.toList
  }

  private def stringToMethodInfo(str_id: String, methodSummary: MethodSummary) : Unit = {
    val tokens = str_id.split(" ")
    methodSummary.returnType = tokens(0)
    val methodDescription = tokens(1)
    methodSummary.methodName = methodDescription
  }



}
