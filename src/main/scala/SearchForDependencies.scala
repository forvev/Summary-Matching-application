import org.json4s.jackson.JsonMethods.{pretty, render}
import org.opalj.br.analyses.Project
import org.opalj.br.instructions._
import org.opalj.br._
import play.api.libs.json.{JsObject, Json}
import org.json4s.{Formats, jackson}
import org.json4s.jackson.{JsonMethods, parseJson}

import java.nio.file.{FileSystems, Files, Paths, StandardOpenOption}
import java.io.File
import scala.collection.mutable
import scala.reflect.internal.util.FileUtils
import scala.reflect.runtime.universe.{runtimeMirror, typeOf}
import scala.xml.XML.encoding

class SearchForDependencies(var xml_urls_path: String, var jar_path: String) {
  val project = Project(new File(jar_path))
  //hashmap for dependencies
  var classWithDependencies: mutable.Map[String, mutable.HashSet[String]] = mutable.HashMap()
  var classWithMatchSummary: mutable.Map[String, ClassMatchSummary] = mutable.HashMap()
  var classSummaries: mutable.HashSet[ClassSummary] = mutable.HashSet()

  def execute(): Unit = {
    val xml_urls_dir = FileSystems.getDefault.getPath(xml_urls_path)

    //read every summary inside the files and search for match summaries
    Files.list(xml_urls_dir).forEach(path => {
      val readSummary = new ReadSummary(path.toString)

      val classSummary = readSummary.getClassSummary()
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
    val path_json_with_dependencies = Paths.get("./src/main/JSON/match_dependencies.json")
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


  //  def main(args: Array[String]): Unit = {
  //    val xml_urls_dir = FileSystems.getDefault.getPath(xml_urls_path)
  //    Files.list(xml_urls_dir).toList.forEach(path => {
  //      val readSummary = new ReadSummary(path.toString)
  //      val classSummary = readSummary.getClassSummary()
  //      checkMatchSummary(classSummary)
  //    })
  //    checkRelationOfSummary()
  //    val searchForDependencies = new SearchForDependencies(classWithDependencies)
  //  }

  // todo list
  // check when A depends on B and B depends on C, A should depend on C
  // for test case we have to use the existing summaries (like Color class
  // https://github.com/secure-software-engineering/FlowDroid/blob/develop/soot-infoflow-summaries/summariesManual/android.graphics.Color.xml)
  // xml file of summary that we map
  // change xml file (summary) into json file
  // json file where we include dependencies of summary

}
