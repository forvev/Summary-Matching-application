import scala.xml.XML
import org.opalj.ai.domain.l1.DefaultDomainWithCFGAndDefUse
import org.opalj.br.analyses.Project
import org.opalj.br.instructions._
import org.opalj.br._
import scala.xml.XML


import java.io.File
import scala.xml.NodeSeq.seqToNodeSeq

object Test {
  val xml_url = "C:/Users/tam20/SoftwareDevelopmentProject/src/main/xml-files/PatternMatcher.xml"
  val projectJAR = "C:/Users/tam20/SoftwareDevelopmentTool/src/main/jar-files/app-debug.jar"
  val projectJAR2 = "C:/Users/tam20/SoftwareDevelopmentProject/src/main/jar-files/STD_app.jar"
  val project = Project(new File(projectJAR2))

  def checkMethod(methods_of_summary: List[MethodSummary]) : Unit = {
    project.allMethodsWithBody.foreach(method => {
      println(method.name)
      println("method length: " + method.parameterTypes.length)
      val signature = method.signature.toJava
      val parameters = signature.substring(signature.lastIndexOf("(")+1, signature.lastIndexOf(")"))
      println("return type: " + method.returnType.toJava)
      println("end method")
    })
  }

  def main(args: Array[String]): Unit = {
    val readSummary = new ReadSummary(xml_url)
    val methods = readSummary.getMethods()
    for (elem <- methods) {
      println(elem.toResult())
    }
    checkMethod(methods)
  }

}
