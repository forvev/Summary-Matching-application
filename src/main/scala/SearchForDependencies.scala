import scala.xml.XML
import org.opalj.ai.domain.l1.DefaultDomainWithCFGAndDefUse
import org.opalj.br.analyses.Project
import org.opalj.br.instructions._
import org.opalj.br._
import play.api.libs.json.Json

import java.nio.file.{FileSystems, Files, Paths, StandardOpenOption}
import java.io.File
import scala.collection.mutable

class SearchForDependencies(var xml_urls_path: String, var jar_path: String) {
  val project = Project(new File(jar_path))
  //hashmap for dependencies
  var classWithDependencies : mutable.Map[String, mutable.HashSet[String]] = mutable.HashMap()
  var classWithMatchSummary: mutable.Map[String, ClassMatchSummary] = mutable.HashMap()

  def execute : Unit = {
    val xml_urls_dir = FileSystems.getDefault.getPath(xml_urls_path)

    //read every summary inside the files and search for match summaries
    Files.list(xml_urls_dir).forEach(path => {
      val readSummary = new ReadSummary(path.toString)

      val classSummary = readSummary.getClassSummary()
      checkMatchSummary(classSummary)
    })
    checkRelationOfSummary()

    //If the file exists already, delete it
    if (Files.exists(Paths.get("./src/main/JSON/match_summaries.json"))){
      Files.deleteIfExists(Paths.get("./src/main/JSON/match_summaries.json"))
    }

    //create a new JSON file with the summaries
    val path = Paths.get("./src/main/JSON/match_summaries.json")
    Files.createFile(path)
    classWithMatchSummary.foreach(u => {
      val data = Json.obj("Class_name" -> u._1, "Summary_name" -> u._2.summary_Name)
      val json = data.toString()
      Files.write(Paths.get("./src/main/JSON/match_summaries.json"), (json+"\n").getBytes(), StandardOpenOption.APPEND)
    })

    //dependencies part

  }

  def writeMatchDependenciesInJson() : Unit = {
    //----------creating a json file for XMLs------------
    // if the XML exists delete it
    Files.deleteIfExists(Paths.get("./src/main/JSON/match_dependencies.json"))

    //create a new JSON file with the dependencies
    val path_json_with_dependencies = Paths.get("./src/main/JSON/match_dependencies.json")
    Files.createFile(path_json_with_dependencies)
    classWithDependencies.foreach(u => {
      val data = Json.obj("Class_name" -> u._1, "dependent_class/es" -> u._2)
      val json = data.toString()
      Files.write(Paths.get("./src/main/JSON/match_dependencies.json"), (json + "\n").getBytes(), StandardOpenOption.APPEND)
    })
  }

  def checkMatchSummary(classSummary: ClassSummary) : Unit = {
    //go through all of the classes in the project
    project.allProjectClassFiles.foreach(specific_class =>{
      val number_of_method = specific_class.methods.length
      var real_number_of_method: Int = 0
      val methods_of_summary = classSummary.getMethods()

      specific_class.methods.foreach(method =>{
        val parameterLength = method.parameterTypes.length

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
    classWithMatchSummary.foreach(summary =>{
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
