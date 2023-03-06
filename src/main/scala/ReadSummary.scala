import scala.collection.mutable.ListBuffer
import scala.xml.XML
import org.json4s.Xml.toJson
import org.json4s.jackson.JsonMethods.{pretty, render}
import spray.json.DefaultJsonProtocol.immSeqFormat

import java.nio.file.{Files, Paths, StandardOpenOption}
import spray.json._

class ReadSummary(var xml_url: String) {

  val xml = XML.loadFile(xml_url)

  //create a json structure, but with basic view (curly brackets and so on)
  val data = toJson(xml)

  //change the view to typical json view
  val data_2 = pretty(render(data))
  val json = data_2


  val temp = (xml \\ "summary" \\ "methods " \ "method")
  val className = xml_url.substring(xml_url.lastIndexOf('\\')+1, xml_url.lastIndexOf('.'))

  //save json to the file
  val path_as_string = "./src/main/summaries_as_json/" + className + ".json"
  val path = Paths.get(path_as_string)
  Files.deleteIfExists(path)
  Files.createFile(path)
  Files.write(path, (json + "\n").getBytes(), StandardOpenOption.APPEND)

  def getClassSummary(): ClassSummary = {
    var methods = new ListBuffer[MethodOfSummary]()
    var result = new ClassSummary(className)
    (xml \\ "summary" \\ "methods" \\ "method" \\ "@id").foreach(method => {
      var methodSummary = new MethodOfSummary()
      stringToMethodInfo(method.toString(), methodSummary)
      methods.append(methodSummary)
    })
    result.setMethods(methods)
    result
  }

  private def stringToMethodInfo(str_id: String, methodSummary: MethodOfSummary) : Unit = {
    val tokens = str_id.split(" ")
    methodSummary.returnType = tokens(0)
    val methodDescription = tokens(1)
    val parameters_as_string = methodDescription.substring(methodDescription.lastIndexOf('(') + 1, methodDescription.lastIndexOf(')'))
    if (parameters_as_string.isEmpty)
      methodSummary.parametersLength = 0
    else {
      methodSummary.methodParameters = parameters_as_string.split(',')
      methodSummary.parametersLength = methodSummary.methodParameters.length
    }
  }



}
