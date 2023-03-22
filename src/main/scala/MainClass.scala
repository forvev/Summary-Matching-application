object MainClass {
  def main(args: Array[String]) = {
    val xml_urls_path = "./src/main/resources/xml-files"
    val projectJAR2 = "./src/main/resources/jar-files/classes.jar"
    val object_to_test = new SearchForDependencies(xml_urls_path = xml_urls_path, jar_path = projectJAR2)
    object_to_test.execute
  }
}
