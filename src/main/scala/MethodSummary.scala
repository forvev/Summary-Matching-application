

class MethodSummary() {
  var methodName: String = _
  var methodParameters: String = _
  var parametersLength: Int = _
  var returnType: String = _

  def toResult(): String = {
    val result = returnType + " " + parametersLength + " " + methodParameters
    result
  }
}
