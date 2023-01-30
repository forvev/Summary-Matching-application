import org.opalj.br.ClassFile

import scala.collection.mutable


class ClassMatchSummary(var className: String, var summary_Name: String) {

  var classesAreCalledByThisClass: mutable.HashSet[ClassMatchSummary] = mutable.HashSet()
  var classesCallThisClass: mutable.HashSet[ClassMatchSummary] = mutable.HashSet()

  def addClassIsCalledByThisClass(classMatchSummary: ClassMatchSummary) : Unit = {
    classesAreCalledByThisClass.add(classMatchSummary)
    classMatchSummary.addClassCallsThisClass(this)
  }

  private def addClassCallsThisClass(classMatchSummary: ClassMatchSummary): Unit = {
    classesCallThisClass.add(classMatchSummary)
  }

  def getListOfDependencies(dependencies: mutable.HashSet[String], classSummary: ClassMatchSummary = null): Unit = {
    if (classesAreCalledByThisClass.isEmpty)  return
    classesAreCalledByThisClass.foreach(f => {
      if (!dependencies.contains(f.className)){
        dependencies.add(f.className)
        f.getListOfDependencies(dependencies)
      }
    })
  }
}

