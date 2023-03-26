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
import java.io.File
import java.nio.file._
import scala.collection.JavaConverters._
import java.net.{URI, URL}
import java.util.stream._
import scala.tools.nsc.Main

/**
 *
 * @Authors: Artur, Tam
 */

class SearchForDependencies(var jar_path: String, var summaries_path: String, var output_path: String) {
  implicit val project = Project(
    new File(jar_path), // path to the JAR files/directories containing the project
    org.opalj.bytecode.RTJar // predefined path(s) to the used libraries
  )
  //hashmap for dependencies
  var classWithDependencies: mutable.Map[String, mutable.HashSet[String]] = mutable.HashMap()
  var classWithMatchSummary: mutable.Map[String, ClassMatchSummary] = mutable.HashMap()
  var classSummaries: mutable.HashSet[ClassSummary] = mutable.HashSet()

  def execute(): Unit = {
    val path = Paths.get(summaries_path)
    val summaries_as_json_path = Paths.get(output_path, "xml_as_json")
    val ls = Files.list(path)

    ls.forEach(path=>{
      val readSummary = new ReadSummary(path.toString, summaries_as_json_path)
      val classSummary = readSummary.get_classSummary()
      classSummaries.add(classSummary)
    })

    project.allProjectClassFiles.foreach(specific_class => {
      classSummaries.foreach(classSummary => {
        checkMatchSummary(classSummary, specific_class)
      })
    })
    checkRelationOfSummary()
    writeMatchDependenciesInJson()
    writeMatchSummary()
  }



  def writeMatchSummary(): Unit = {
    //If the file exists already, delete it
    val path = Paths.get(output_path,"match_summaries.json")
    Files.deleteIfExists(path)

    //create a new JSON file with the summaries
    Files.createFile(path)
    var result = new StringBuilder("[ ")
    classWithMatchSummary.foreach(u => {
      var match_summaries = u._2.matchesSummariestoJson()
      val data = Json.obj("Class_name" -> u._1, "match_summaries" -> match_summaries)
      val json = pretty(parseJson(data.toString()))
      result.append(json + ", \n")
    })
    result.replace(result.lastIndexOf(','), result.length - 1, " ]")
    Files.write(path, result.toString().getBytes(), StandardOpenOption.APPEND)

  }

  def writeMatchDependenciesInJson(): Unit = {
    //----------creating a json file for XMLs------------
    // if the XML exists delete it
    val path_json_with_dependencies = Paths.get(output_path,"match_dependencies.json")
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
    specific_class.methods.foreach(method => {
      method.parameterTypes.foreach(p_type => {
        checkDependencies(result, p_type.toJava)
      })
      if (method.body.isDefined){
        val code = method.body.get
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
      }
    })
    result
  }

  def checkDependencies(result: mutable.HashSet[String], className: String): Unit = {
    result.add(className)
  }

  def checkRelationOfSummary(): Unit = {
    classWithMatchSummary.foreach(summary => {
      classWithDependencies.foreach(dependencies => {
        if (!summary._2.className.equals(dependencies._1)) {
          dependencies._2.foreach(className => {
            if (className.equals(summary._1)) {
              classWithMatchSummary(dependencies._1).addClassIsCalledByThisClass(summary._2)
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
