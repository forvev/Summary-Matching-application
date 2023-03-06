import org.opalj.br._

import scala.collection.mutable.ListBuffer

class MethodOfSummary() {
  var methodParameters: Array[String] = _
  var parametersLength: Int = _
  var returnType: String = _

  def isMatched(method: Method): Boolean = {
    if (returnType != method.returnType.toJava || parametersLength != method.parameterTypes.length) {
      return false
    }
    if (parametersLength == 0 && method.parameterTypes.length == 0)
      return true
    var parameter_types = methodParameters.toBuffer
    for (param_type <- method.parameterTypes.toList){
      val t = param_type.toJava
      if (parameter_types.filter(p => p.equals(t)).isEmpty){
        return false
      } else {
        parameter_types -= t
      }
    }
    true
  }
  
  
}
