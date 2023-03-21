import scala.collection.mutable.ListBuffer
import scala.xml.{Elem, XML}
import org.json4s.Xml.toJson
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.{DefaultFormats, _}

import java.nio.file.{Files, Paths, StandardOpenOption}
import spray.json._


class ReadSummary(var url: String) {

  val className = url.substring(url.lastIndexOf('\\')+1, url.lastIndexOf('.'))

  def read_json_file(): ClassSummary = {
    var result = new ClassSummary(className)
    var methods = new ListBuffer[MethodOfSummary]()
    implicit val formats = DefaultFormats
    val json_content = scala.io.Source.fromFile(url).mkString.stripMargin
    val json_data = parse(json_content)
    val summary_methods = (json_data \ "summary" \ "methods" \ "method" \ "id").children
    summary_methods.foreach(method => {
      var methodSummary = new MethodOfSummary()
      stringToMethodInfo(method.asInstanceOf[JString].s, methodSummary)
      methods.append(methodSummary)
    })
    result.setMethods(methods)
    result
  }

  def get_classSummary(): ClassSummary ={
    if (url.endsWith(".xml"))
      read_xml_file()
    else
      read_json_file()

  }


  private def read_xml_file(): ClassSummary ={
    val xml = XML.loadFile(url)

    //create a json structure, but with basic view (curly brackets and so on)
    val data = toJson(xml)

    //change the view to typical json view
    val data_2 = pretty(render(data))
    val json = data_2


    val temp = (xml \\ "summary" \\ "methods " \ "method")


    //save json to the file
    val path_as_string = "./src/main/summaries_as_json/" + className + ".json"
    val path = Paths.get(path_as_string)
    Files.deleteIfExists(path)
    Files.createFile(path)
    Files.write(path, (json + "\n").getBytes(), StandardOpenOption.APPEND)
    getClassSummaryWithXMLFile(xml, className)
  }

  private def getClassSummaryWithXMLFile(xml: Elem, className: String): ClassSummary = {
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
