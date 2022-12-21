class ClassSummary(var className: String){
  private var methods : List[MethodSummary] = _

  def setMethods(methods: List[MethodSummary]): Unit = {
    this.methods = methods
  }

  def getMethods(): List[MethodSummary] = {
    this.methods
  }
}
