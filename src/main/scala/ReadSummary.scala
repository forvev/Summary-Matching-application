import scala.collection.mutable.ListBuffer
import scala.xml.{Elem, XML}
import org.json4s.Xml.toJson
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.{DefaultFormats, _}

import java.nio.file.{Files, Paths, StandardOpenOption}

class ReadSummary(var url: String) {

  val className = url.substring(url.lastIndexOf('\\')+1, url.lastIndexOf('.'))

  def read_json_file(): ClassSummary = {
    var result = new ClassSummary(className)
    var methods = new ListBuffer[MethodOfSummary]()
    implicit val formats = DefaultFormats
    val source = scala.io.Source.fromFile(url)
    val json_content = source.mkString.stripMargin
    source.close()
    val json_data = parse(json_content)
    val summary_methods = (json_data \ "summary" \ "methods" \ "method" \ "id").children
    summary_methods.foreach(method => {
      var methodSummary = new MethodOfSummary(className, method.asInstanceOf[JString].s)
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
    val xml = XML.load(url)
    //create a json structure, but with basic view (curly brackets and so on)
    val data = toJson(xml)

    //change the view to typical json view
    val data_2 = pretty(render(data))
    val json = data_2
    transform_xml_to_json(json)
    getClassSummaryWithXMLFile(xml, className)
  }

  private def getClassSummaryWithXMLFile(xml: Elem, className: String): ClassSummary = {
    var methods = new ListBuffer[MethodOfSummary]()
    var result = new ClassSummary(className)
    (xml \\ "summary" \\ "methods" \\ "method" \\ "@id").foreach(method => {
      var methodSummary = new MethodOfSummary(className, method.toString())
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

  private def transform_xml_to_json(data: String): Unit = {
    //save json to the file
    val xml_as_json_path = "./src/main/resources/xml_as_json"
    val dir_path = Paths.get(xml_as_json_path)
    if (!Files.isDirectory(dir_path))
      Files.createDirectory(dir_path)
    else {
      val path_as_string = className + ".json"
      val path = Paths.get(xml_as_json_path, path_as_string)
      Files.deleteIfExists(path)
      Files.createFile(path)
      Files.write(path, (data + "\n").getBytes(), StandardOpenOption.APPEND)
    }
  }



}
