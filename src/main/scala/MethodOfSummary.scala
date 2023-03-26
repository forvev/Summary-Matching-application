import org.opalj.br._

import scala.collection.mutable.ListBuffer

/**
 *
 * @Authors: Tam
 */

class MethodOfSummary(var className: String, var method_string: String) {
  var methodParameters: Array[String] = _
  var parametersLength: Int = _
  var returnType: String = _

  def isMatched(method: Method): Boolean = {
    if (!check_type(returnType, method.returnType.toJava, method) || parametersLength != method.parameterTypes.length) {
      return false
    }
    if (parametersLength == 0 && method.parameterTypes.length == 0)
      return true
    var parameter_types = methodParameters.toBuffer
    for (param_type <- method.parameterTypes.toList){
      val t = param_type.toJava
      if (parameter_types.filter(p => check_type(p,t, method)).isEmpty){
        return false
      } else {
        parameter_types -= t
      }
    }
    true
  }

  private def check_type(summary_type: String, jar_type: String, method: Method) : Boolean = {
    if (summary_type.equals(jar_type))
      return true
    val classFileName = method.classFile.fqn.replace("/", ".")
    if (summary_type.equals(className) && jar_type.equals(classFileName))
      return true
    false
  }
  
  
}
