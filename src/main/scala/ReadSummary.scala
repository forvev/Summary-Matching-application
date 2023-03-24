import scala.collection.mutable.ListBuffer
import scala.xml.{Elem, NodeSeq, XML}
import org.json4s.Xml.toJson
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.{DefaultFormats, _}

import java.io.File
import java.net.{URI, URL}
import java.nio.file.{FileSystems, Files, Paths, StandardOpenOption}
import java.nio.file._
import scala.collection.JavaConverters.asJavaIterableConverter
import scala.io.Source
import scala.tools.nsc.Main
import scala.collection.JavaConverters._
import scala.xml.factory.XMLLoader



class ReadSummary(var url: String) {
  //private var jarFS: FileSystem = null; // Static variable for storing a FileSystem. Will be loaded on the first call to getPath.
  val isJar = getClass.getResource("summary-files").toString.contains(".jar")

  //  val parts = url.split("/")
//  val result = parts.last
//
//  val className_2 = getClass().getResourceAsStream("summary-files/" + result)
//  //println("test: " + className)
//  println("the s: "+className_2.read().getClass.getName)
//  println("before:"+url)
  val className = url.substring(url.lastIndexOf('\\')+1, url.lastIndexOf('.'))

  def read_json_file(): ClassSummary = {
    var result = new ClassSummary(className)
    var methods = new ListBuffer[MethodOfSummary]()
    implicit val formats = DefaultFormats

    var source : Source = null
    if (isJar) {
      val parts = url.split("/")
      val result = parts.last

      val my_path = new File(".").getAbsolutePath.dropRight(1)
      source = scala.io.Source.fromFile(my_path+result)
    }
    else {
      source = scala.io.Source.fromFile(url)
    }

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

    var xml: Elem = null
    if (isJar) {
      val parts = url.split("/")
      val result = parts.last
      val url_resources = getClass().getResourceAsStream("/summary-files/" + result)
      xml = XML.load(url_resources)
    }
    else {
      xml = XML.load(url)
    }

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
    var xml_as_json_path: URL = null
    var xml_as_json_path_2: String = null
    var dir_path: Path = null
    if (isJar) {
      xml_as_json_path = Main.getClass.getResource("/summary-files")
      dir_path = Paths.get(xml_as_json_path.toURI) //getPath(xml_as_json_path)

      val parts = className.split("/")
      val result = parts.last
      val path_as_string = result + ".json"

      val my_path = new File(".").getAbsolutePath
      val generatedFile = new File(my_path, path_as_string)
      generatedFile.createNewFile();
      val path_2 = Paths.get(my_path)
      Files.write(generatedFile.toPath, (data + "\n").getBytes(), StandardOpenOption.APPEND)

    }
    else{
      xml_as_json_path_2 = "./src/main/resources/xml_as_json"
      dir_path = Paths.get(xml_as_json_path_2)

      if (!Files.isDirectory(dir_path)) {
        Files.createDirectory(dir_path)
      } else {
        val parts = className.split("/")
        val result = parts.last
        val path_as_string = result + ".json"
        val path = Paths.get(xml_as_json_path_2, path_as_string)
        Files.deleteIfExists(path)
        Files.createFile(path)
        Files.write(path, (data + "\n").getBytes(), StandardOpenOption.APPEND)
      }
    }
  }

  private var jarFS: FileSystem = null; // Static variable for storing a FileSystem. Will be loaded on the first call to getPath.

  def getPath(url: URL): Path = {
    if (url.getProtocol == "file") {
      Paths.get(url.toURI)
    } else {
      // This hacky branch is to handle reading resource files from a jar (where url is jar:...).
      val strings = url.toString.split("!")
      if (jarFS == null) {
        jarFS = FileSystems.newFileSystem(URI.create(strings(0)), Map[String, String]().asJava)
      }
      jarFS.getPath(strings(1))
    }
  }
}
