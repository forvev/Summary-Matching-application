import org.opalj.br._

import scala.collection.mutable.ListBuffer


class ClassSummary(var className: String){
  private var methods : ListBuffer[MethodOfSummary] = _
  private var match_probability: Float = 0


  def setMethods(methods: ListBuffer[MethodOfSummary]): Unit = {
    this.methods = methods
  }

  def getMethods(): ListBuffer[MethodOfSummary] = {
    this.methods
  }

  def isMatched(classFile: ClassFile): Boolean = {
    var summary_methods: ListBuffer[MethodOfSummary] = methods.clone( )
    val length_summary_methods = methods.length
    val length_class_file_methods = classFile.methods.length
    if (length_summary_methods > length_class_file_methods) {
      return false
    }
    classFile.methods.map(method => {
      var after_filter = summary_methods.filter(m => m.isMatched(method))
      if (after_filter.nonEmpty)
        summary_methods -= after_filter.head
    })
    //println(length_summary_methods+" div by " + length_class_file_methods)
    match_probability = length_summary_methods.toFloat /length_class_file_methods.toFloat
    summary_methods.isEmpty
  }

  override def clone(): ClassSummary = {
    var result = new ClassSummary(this.className)
    result.match_probability = this.match_probability
    result
  }

  def get_match_probability(): Float = {
    match_probability
  }


}
