import org.json4s.DoubleJsonFormats.GenericFormat
import org.json4s.jackson.JsonMethods.{pretty, render}

import scala.xml.XML
import org.opalj.ai.domain.l1.DefaultDomainWithCFGAndDefUse
import org.opalj.br.analyses.Project
import org.opalj.br.instructions._
import org.opalj.br._
import play.api.libs.json.Json
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
  //val project = ClassFiles(Paths.get(jar_path))
  //hashmap for dependencies
  var classWithDependencies : mutable.Map[String, mutable.HashSet[String]] = mutable.HashMap()
  var classWithMatchSummary: mutable.Map[String, ClassMatchSummary] = mutable.HashMap()

  def execute : Unit = {
    val xml_urls_dir = FileSystems.getDefault.getPath(xml_urls_path)

    println("dddd")
    //----------creating a json file for XMLs------------
    // if the XML exists delete it
    if (Files.exists(Paths.get("./src/main/JSON/xml_files.json"))) {
      Files.deleteIfExists(Paths.get("./src/main/JSON/xml_files.json"))
    }
    //create a new json file
    val path_2 = Paths.get("./src/main/JSON/xml_files.json")
    Files.createFile(path_2)


    //read every summary inside the files and search for match summaries
    Files.list(xml_urls_dir).forEach(path => {
      val readSummary = new ReadSummary(path.toString)

      val classSummary = readSummary.getClassSummary()
      checkMatchSummary(classSummary)
    })
    checkRelationOfSummary()

    writeMatchDependenciesInJson()
    writeMatchSummary()
  }

  def writeMatchSummary() : Unit = {
    //If the file exists already, delete it
    Files.deleteIfExists(Paths.get("./src/main/JSON/match_summaries.json"))

    //create a new JSON file with the summaries
    val path = Paths.get("./src/main/JSON/match_summaries.json")
    Files.createFile(path)
    var result = new StringBuilder("[")
    classWithMatchSummary.foreach(u => {
      val data = Json.obj("Class_name" -> u._1, "Summary_name" -> u._2.summary_Name)
      result.append(data.toString() + ", \n")
    })
    result.replace(result.lastIndexOf(','), result.length-1, "]")
    Files.write(Paths.get("./src/main/JSON/match_summaries.json"), result.toString().getBytes(), StandardOpenOption.APPEND)
  }

  def writeMatchDependenciesInJson() : Unit = {
    //----------creating a json file for XMLs------------
    // if the XML exists delete it
    val path_json_with_dependencies = Paths.get("./src/main/JSON/match_dependencies.json")
    Files.deleteIfExists(path_json_with_dependencies)

    //create a new JSON file with the dependencies
    var result = new StringBuilder("[")
    classWithMatchSummary.foreach(u => {
      val dependencies = mutable.HashSet[String]()
      u._2.getListOfDependencies(dependencies, u._1)
      val data = Json.obj("Class_name" -> u._1, "Name_of_match_Summary" -> u._2.summary_Name, "depend_on_classes" -> dependencies )
      val json = pretty(parseJson(data.toString()))
      result.append(json + ", \n")
    })
    result.replace(result.lastIndexOf(','), result.length-1, "]")
    Files.write(path_json_with_dependencies, result.toString.getBytes())
  }



  def checkMatchSummary(classSummary: ClassSummary) : Unit = {
    //go through all of the classes in the project
    project.allProjectClassFiles.foreach(specific_class =>{
      val number_of_method = specific_class.methods.length
      var real_number_of_method: Int = 0
      //we will get names of the method (type, num. of parameters, types of parameters)
      val methods_of_summary = classSummary.getMethods()

      val fields = specific_class.fields
//      println("project class: "+specific_class)
//      fields.foreach(fields_inside =>{
//        // we want to consider only objects (it will discard all of the primitive types)
//        if (fields_inside.fieldType.isObjectType){
//
//          println("class: "+fields_inside+" methods: "+ fields_inside.fieldType.getClass)
//
//          val objectType = ObjectType("java.lang.String")
//          val classFile = project.classFile(objectType).getOrElse(throw new RuntimeException(s"Class $objectType not found"))
//          println("new methods: "+classFile)
//
//        }
//        // classOf[Field].cast(fields_inside).fieldType == fields_inside.fieldType
//        // .getClass.getDeclaredMethods.foreach(x=>{
//        //            println(x.toString)
//        //          })
//      })






      //go through each method inside the specific class
      specific_class.methods.foreach(method =>{
        //take the length of the parameters of the method/function
        val parameterLength = method.parameterTypes.length

        //there are the arguments inside the method
        val signature = method.signature.toJava
        var parameters = signature.substring(signature.lastIndexOf("(") + 1, signature.lastIndexOf(")"))
        if (parameters == "")
          parameters = "null"

        val returnType = method.returnType.toJava
        var result = returnType + " " + parameterLength + " " + parameters
        if (methods_of_summary.exists(p => p.toResult().equals(result))) real_number_of_method+=1

      })

      //If the number of methods in the specific class is the same as in the summary...
      if(real_number_of_method==number_of_method){
        println("Pattern has been found!\nWith class: "+specific_class.methods)
        //we need to store information about existing summaries for future use(we will use this list of class to check dependencies)
        val className = specific_class.fqn.replace("/", ".")

        //search inside fields of class or methods
        classWithDependencies += ( className -> getCalledClasses(specific_class))

        var classMatchSummary = new ClassMatchSummary(className, classSummary.className)
        classWithMatchSummary += ( className -> classMatchSummary)
      }
    })
  }



  def getCalledClasses(specific_class: ClassFile) : mutable.HashSet[String] = {
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
        case _ =>
      }
    })
    result
  }

  def checkDependencies(result: mutable.HashSet[String], className: String) : Unit = {
    val primitiveTypes = List("int", "boolean", "char", "double", "float", "short", "long", "byte")
    if (className.startsWith("java.lang.") || primitiveTypes.contains(className)) {
      return
    }
    result.add(className)
  }

  def checkRelationOfSummary(): Unit = {
    println("----------------------------------------------")
    println("dep: "+classWithDependencies)
    classWithMatchSummary.foreach(summary =>{
      //println("summary: "+summary._2.className)
      classWithDependencies.foreach(dependencies =>{
        if (!summary._2.className.equals(dependencies._1)){
          dependencies._2.foreach(className => {
            if (className.equals(summary._1)){
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
