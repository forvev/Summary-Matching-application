import scala.xml.XML
import org.opalj.ai.domain.l1.DefaultDomainWithCFGAndDefUse
import org.opalj.br.analyses.Project
import org.opalj.br.instructions._
import org.opalj.br._
import java.nio.file.{FileSystems, Files}

import scala.xml.XML
import java.io.File
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Test {
  val xml_urls_path = "./src/main/xml-files"
  val projectJAR2 = "./src/main/jar-files/STD_app2.jar"
  val project = Project(new File(projectJAR2))
  //hashmap for dependencies
  var classWithDependencies : mutable.Map[String, mutable.HashSet[String]] = mutable.HashMap()
  var classWithMatchSummary: mutable.Map[String, String] = mutable.HashMap()

  def checkMatchSummary(classSummary: ClassSummary) : Unit = {
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
        classWithDependencies += (specific_class.fqn.replace("/", ".") -> getCalledClasses(specific_class))
        classWithMatchSummary += (specific_class.fqn.replace("/", ".") -> classSummary.className)
      }
    })
  }

  def getCalledClasses(specific_class: ClassFile) : mutable.HashSet[String] = {
    var result = new mutable.HashSet[String]()
    specific_class.fields.foreach(field => {
      // filter java native type
      result.add(field.fieldType.toJava)
    })
    specific_class.methodBodies.foreach(code => {
      code.instructions.foreach {
        case loadClass: LoadClass =>
          result.add(loadClass.value.toJava)
        case loadClass_W: LoadClass_W =>
          result.add(loadClass_W.value.toJava)
        case getStatic: GETSTATIC =>
          result.add(getStatic.declaringClass.toJava)
        case invokeStatic: INVOKESTATIC =>
          result.add(invokeStatic.declaringClass.toJava)
        case anewArray: ANEWARRAY =>
          result.add(anewArray.arrayType.toJava)
        case getField: GETFIELD =>
          result.add(getField.declaringClass.toJava)
        case _ =>
      }
    })
    result
  }

  def checkRelationOfSummary(): Unit = {
    println("----------------------------------------------")
    classWithMatchSummary.keys.foreach(summary =>{
      classWithDependencies.foreach(dependencies =>{
        //("key: "+key+" dep: "+dependencies._1)
        if (!summary.equals(dependencies._1)){
          dependencies._2.foreach(className => {
            if (className.equals(summary)){
              println("class " + dependencies._1 + " calls the class " + summary)
            }
          })
            //println("I've found the dependencies between:\n"+key+"\nand\n"+dependencies._1)
        }
      })
    })
  }


  def main(args: Array[String]): Unit = {
    val xml_urls_dir = FileSystems.getDefault.getPath(xml_urls_path)
    Files.list(xml_urls_dir).toList.forEach(path => {
      val readSummary = new ReadSummary(path.toString)
      val classSummary = readSummary.getClassSummary()
      checkMatchSummary(classSummary)
    })
    checkRelationOfSummary()
    val searchForDependencies = new SearchForDependencies(classWithDependencies)
  }

}
