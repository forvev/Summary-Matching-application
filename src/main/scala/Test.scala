import scala.xml.XML
import org.opalj.ai.domain.l1.DefaultDomainWithCFGAndDefUse
import org.opalj.br.analyses.Project
import org.opalj.br.instructions._
import org.opalj.br._
import scala.xml.XML


import java.io.File
import scala.xml.NodeSeq.seqToNodeSeq

object Test {
  val xml_url = "C:/Users/Artur/Desktop/studia/V semestr/Software Development Tools/Scala_project_2/src/main/xml-files/PatternMatcher.xml"
  val projectJAR = "C:/Users/Artur/Desktop/studia/V semestr/Software Development Tools/Scala_project_2/src/main/jar-files/app-debug.jar"
  val projectJAR2 = "C:/Users/Artur/Desktop/studia/V semestr/Software Development Tools/Scala_project_2/src/main/jar-files/STD_app2.jar"
  val project = Project(new File(projectJAR2))
  var result = ""

  def checkMethod(methods_of_summary: List[MethodSummary]) : Unit = {
    project.allProjectClassFiles.foreach(specific_class =>{

      val number_of_method = specific_class.methods.length
      var real_number_of_method: Int = 0

      specific_class.methods.foreach(method =>{
        val parameterLength = method.parameterTypes.length

        val signature = method.signature.toJava
        var parameters = signature.substring(signature.lastIndexOf("(") + 1, signature.lastIndexOf(")"))
        if (parameters == "")
          parameters = "null"
        val returnType = method.returnType.toJava
        result = returnType + " " + parameterLength + " " + parameters
        if (methods_of_summary.exists(p => p.toResult().equals(result))) real_number_of_method+=1
      })

      //If the number of methods in the specific class is the same as in the summary...
      if(real_number_of_method==number_of_method){
        println("Pattern has been found!\nWith class: "+specific_class.methods)

      }
    })

  }

  def main(args: Array[String]): Unit = {
    val readSummary = new ReadSummary(xml_url)
    val methods = readSummary.getMethods()
//    for (elem <- methods) {
//      println("dd"+elem.toResult())
//    }
    checkMethod(methods)
  }

}
