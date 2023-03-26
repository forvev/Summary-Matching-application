import org.opalj.br.ClassFile
import play.api.libs.json.{JsObject, Json}

import scala.collection.mutable

/**
 *
 * @Authors: Artur, Tam
 */

class ClassMatchSummary(var className: String) {

  var matchSummaries: mutable.HashSet[ClassSummary] = mutable.HashSet[ClassSummary]()
  var classesAreCalledByThisClass: mutable.HashSet[ClassMatchSummary] = mutable.HashSet()
  var classesCallThisClass: mutable.HashSet[ClassMatchSummary] = mutable.HashSet()

  def addClassIsCalledByThisClass(classMatchSummary: ClassMatchSummary) : Unit = {
    classesAreCalledByThisClass.add(classMatchSummary)
    classMatchSummary.addClassCallsThisClass(this)
  }

  private def addClassCallsThisClass(classMatchSummary: ClassMatchSummary): Unit = {
    classesCallThisClass.add(classMatchSummary)
  }

  def getListOfDependencies(dependencies: mutable.HashSet[String], className: String): Unit = {
    if (classesAreCalledByThisClass.isEmpty)  return
    classesAreCalledByThisClass.foreach(f => {
      if (!dependencies.contains(f.className) && f.className != className){
        dependencies.add(f.className)
        f.getListOfDependencies(dependencies, className)
      }
    })
  }

  def matchesSummariestoJson(): mutable.HashSet[JsObject] = {
    var match_summaries = mutable.HashSet[JsObject]()
    matchSummaries.foreach(s => {
      val info = Json.obj("Summary_name" -> s.className, "match_probability" -> s.get_match_probability())
      match_summaries.add(info)
    })
    match_summaries
  }

}

