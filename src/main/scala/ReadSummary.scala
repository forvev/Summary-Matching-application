import scala.collection.mutable.ListBuffer
import scala.xml.XML

class ReadSummary(var xml_url: String) {

  val xml = XML.loadFile(xml_url)
  val temp = (xml \\ "summary" \\ "methods " \ "method")
  val className = xml_url.substring(xml_url.lastIndexOf('/')+1, xml_url.lastIndexOf('.'))

  def getClassSummary(): ClassSummary = {
    var methods = new ListBuffer[MethodSummary]()
    var result = new ClassSummary(className)
    (xml \\ "summary" \\ "methods" \\ "method" \\ "@id").foreach(method => {
      var methodSummary = new MethodSummary()
      stringToMethodInfo(method.toString(), methodSummary)
      methods.append(methodSummary)
    })
    result.setMethods(methods.toList)
    result
  }

  private def stringToMethodInfo(str_id: String, methodSummary: MethodSummary) : Unit = {
    val tokens = str_id.split(" ")
    methodSummary.returnType = tokens(0)
    val methodDescription = tokens(1)
    val parameters_as_string = methodDescription.substring(methodDescription.lastIndexOf('(') + 1, methodDescription.lastIndexOf(')'))
    if (parameters_as_string.isEmpty)
      methodSummary.parametersLength = 0
    else {
      //methodSummary.methodParameters = parameters_as_string.split(',')
      methodSummary.methodParameters = parameters_as_string
      methodSummary.parametersLength = parameters_as_string.split(',').length
    }
  }



}
