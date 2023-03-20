import jdk.xml.internal.SecuritySupport.getContextClassLoader
import org.json4s.jackson.JsonMethods.{pretty, render}
import org.opalj.br.analyses.Project
import org.opalj.br.instructions._
import org.opalj.br._
import play.api.libs.json.{JsObject, Json}
import org.json4s.{Formats, jackson}
import org.json4s.jackson.{JsonMethods, parseJson}

import java.nio.file.{FileSystems, Files, Paths, StandardOpenOption}
import java.io.{BufferedReader, File, InputStream}
import java.net.{URLClassLoader, URLDecoder}
import java.util
import scala.collection.mutable
import scala.io.Source
import scala.reflect.internal.util.FileUtils
import scala.reflect.runtime.universe.{runtimeMirror, typeOf}
import scala.tools.jline_embedded.internal.InputStreamReader
import scala.xml.XML
import scala.xml.XML.encoding
import java.nio.file._
import scala.collection.JavaConverters._
import java.net.{URI, URL}
import java.util.stream._
import scala.tools.nsc.Main



class SearchForDependencies(var xml_urls_path: String, var jar_path: String) {
  val project = Project(new File(jar_path))
  //hashmap for dependencies
  var classWithDependencies: mutable.Map[String, mutable.HashSet[String]] = mutable.HashMap()
  var classWithMatchSummary: mutable.Map[String, ClassMatchSummary] = mutable.HashMap()
  var classSummaries: mutable.HashSet[ClassSummary] = mutable.HashSet()

  def execute(): Unit = {
    println("here 1")
    val xml_urls_dir = FileSystems.getDefault.getPath(xml_urls_path)
    println("here 2")


    val xx = new File(".").getAbsolutePath

    println("absolut: "+xx)
//    val xml_folder_2 = getClass.getResource("xml-files")
//    val resourcePath = Paths.get(URLDecoder.decode(xml_folder_2.getFile, "UTF-8")).toString.replace("!","")
//
//    println("path::: "+xml_folder_2)
//    //val xml_folder = Paths.get(xml_folder_2.getPath)
//    val dir_path = new File(resourcePath).listFiles.map(_.getPath)
//    println("path: "+dir_path)
//    println("new path:")
//    dir_path.foreach(d=>{
//      println(d)
//    })

//    val dir_url = ClassLoader.getSystemResource("xml-files")
//    println("location: "+dir_url.toURI)
//    val dir = new File(dir_url.toURI).listFiles.map(_.getPath)

//    val classLoader = getClass.getClassLoader
//    val resourceUrl = classLoader.getResource("xml-files")
//    println("location: "+resourceUrl.toURI)
//    val dir = new File(resourceUrl.toURI).listFiles.map(_.getPath)
//
//    dir.foreach(path=>{
//      println(path)
//    })

    val url = Main.getClass.getResource("/xml-files")
    val path = getPath(url)
    val ls = Files.list(path)

    ls.forEach(path=>{
      println("path: "+path)
      val readSummary = new ReadSummary(path.toString)
      val classSummary = readSummary.getClassSummary()
      classSummaries.add(classSummary)
    })



    //    Files.list(dir_path).forEach(path=>{
//      println(path)
//    })


   // val xml_urls_dir_2 = FileSystems.getDefault.getPath(dir_path.toString)

    //println("d: "+xml_urls_dir_2)


//    val directory = Paths.get(xml_folder_2).toString
//    val xml_folder = new File(directory)

//    println("here 3: "+xml_folder.listFiles().toList)
//
//    xml_folder.listFiles().foreach(path=>{
//      println("path: "+path)
//    })

//    Files.list(xml_urls_dir).forEach(path=>{
//      println(path)
//    })
    println("here 4")


    //val xml_urls_dir = getClass().getClassLoader().getResourceAsStream(xml_urls_path)

    //read every summary inside the files and search for match summaries
//    Files.list(xml_urls_dir).forEach(path => {
//      println("path: "+path)
//      val readSummary = new ReadSummary(path.toString)
//      val classSummary = readSummary.getClassSummary()
//      classSummaries.add(classSummary)
//
//    })


    project.allProjectClassFiles.foreach(specific_class => {

      classSummaries.foreach(classSummary => {
        checkMatchSummary(classSummary, specific_class)
      })
    })
    checkRelationOfSummary()
    writeMatchDependenciesInJson()
    writeMatchSummary()
  }

  // Helper for reading an individual file.
  def readFile(path: Path): String =
    Source.fromInputStream(Files.newInputStream(path), "UTF-8").getLines.mkString("\n")


  private var jarFS: FileSystem = null; // Static variable for storing a FileSystem. Will be loaded on the first call to getPath.

  /**
   * Gets a Path object corresponding to an URL.
   *
   * @param url The URL could follow the `file:` (usually used in dev) or `jar:` (usually used in prod) rotocols.
   * @return A Path object.
   */
  def getPath(url: URL): Path = {
    if (url.getProtocol == "file")
      Paths.get(url.toURI)
    else {
      // This hacky branch is to handle reading resource files from a jar (where url is jar:...).
      val strings = url.toString.split("!")
      if (jarFS == null) {
        jarFS = FileSystems.newFileSystem(URI.create(strings(0)), Map[String, String]().asJava)
      }
      jarFS.getPath(strings(1))
    }
  }

  private def getResourceAsStream(resource: String) = {
    val in = getContextClassLoader.getResourceAsStream(resource)
    if (in == null) getClass.getResourceAsStream(resource)
    else in
  }

  def writeMatchSummary(): Unit = {
    //If the file exists already, delete it
    Files.deleteIfExists(Paths.get("./src/main/JSON/match_summaries.json"))

    //create a new JSON file with the summaries
    val path = Paths.get("./src/main/JSON/match_summaries.json")
    Files.createFile(path)
    var result = new StringBuilder("[ ")
    classWithMatchSummary.foreach(u => {
      var match_summaries = u._2.matchesSummariestoJson()
      val data = Json.obj("Class_name" -> u._1, "match_summaries" -> match_summaries)
      val json = pretty(parseJson(data.toString()))
      result.append(json + ", \n")
    })
    result.replace(result.lastIndexOf(','), result.length - 1, " ]")
    Files.write(Paths.get("./src/main/JSON/match_summaries.json"), result.toString().getBytes(), StandardOpenOption.APPEND)

  }

  def writeMatchDependenciesInJson(): Unit = {
    //----------creating a json file for XMLs------------
    // if the XML exists delete it
    val path_json_with_dependencies = Paths.get("src/main/JSON/match_dependencies.json")
    Files.deleteIfExists(path_json_with_dependencies)

    //create a new JSON file with the dependencies
    var result = new StringBuilder("[ ")
    classWithMatchSummary.foreach(u => {
      val dependencies_as_string = mutable.HashSet[String]()
      u._2.getListOfDependencies(dependencies_as_string, u._1)
      var infos = mutable.HashSet[JsObject]()
      dependencies_as_string.foreach(d => {
        var match_summaries = classWithMatchSummary(d).matchesSummariestoJson()
        val data = Json.obj("Class_name" -> d, "match_summaries" -> match_summaries)
        infos.add(data)
      })
      var match_summaries = u._2.matchesSummariestoJson()
      val data = Json.obj("Class_name" -> u._1, "Name_of_match_Summaries" -> match_summaries, "depend_on_classes" -> infos)
      val json = pretty(parseJson(data.toString()))
      result.append(json + ", \n")
    })
    result.replace(result.lastIndexOf(','), result.length - 1, " ]")
    Files.write(path_json_with_dependencies, result.toString.getBytes())
  }


  def checkMatchSummary(classSummary: ClassSummary, classFile: ClassFile): Unit = {
    //go through all of the classes in the project
    if (classSummary.isMatched(classFile)) {
      println("Pattern has been found!\nWith class: " + classFile.methods)
      val className = classFile.fqn.replace("/", ".")
      classWithDependencies += (className -> getCalledClasses(classFile))
      var classMatchSummary = new ClassMatchSummary(className)
      classMatchSummary.matchSummaries.add(classSummary.clone())
      classWithMatchSummary += ( className -> classMatchSummary)
    }

  }


  def getCalledClasses(specific_class: ClassFile): mutable.HashSet[String] = {
    var result = new mutable.HashSet[String]()
    specific_class.fields.foreach(field => {
      // filter java native type
      checkDependencies(result, field.fieldType.toJava)
    })
    specific_class.methodBodies.foreach(code => {
      code.instructions.foreach {
        case loadClass: LoadClass =>
          checkDependencies(result, loadClass.value.toJava)
        case loadClass_W: LoadClass_W =>
          checkDependencies(result, loadClass_W.value.toJava)
        case getStatic: GETSTATIC =>
          checkDependencies(result, getStatic.declaringClass.toJava)
        case invokeStatic: INVOKESTATIC =>
          checkDependencies(result, invokeStatic.declaringClass.toJava)
        case anewArray: ANEWARRAY =>
          checkDependencies(result, anewArray.arrayType.toJava)
        case getField: GETFIELD =>
          checkDependencies(result, getField.declaringClass.toJava)
        case inst_new: NEW =>
          checkDependencies(result, inst_new.objectType.toJava)
        case invokeSpecial: INVOKESPECIAL =>
          checkDependencies(result, invokeSpecial.declaringClass.toJava)
        case _ =>
      }
    })
    result
  }

  def checkDependencies(result: mutable.HashSet[String], className: String): Unit = {
    result.add(className)
  }

  def checkRelationOfSummary(): Unit = {
    println("----------------------------------------------")
    classWithMatchSummary.foreach(summary => {
      classWithDependencies.foreach(dependencies => {
        if (!summary._2.className.equals(dependencies._1)) {
          dependencies._2.foreach(className => {
            if (className.equals(summary._1)) {
              classWithMatchSummary(dependencies._1).addClassIsCalledByThisClass(summary._2)
              println("class " + dependencies._1 + " calls the class " + summary)
            }
          })
        }
      })
    })
  }

  def get_classWithDependencies_map_size(): Integer = {
    classWithDependencies.size
  }

  def get_classWithMatchSummary_map_size(): Integer = {
    classWithMatchSummary.size
  }


}
