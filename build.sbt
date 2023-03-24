ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.15"
assembly / mainClass := Some("MainClass")
mainClass in Compile := Some("MainClass")

//In oder to create a docker we need to enable following plugins
enablePlugins(JavaAppPackaging)
enablePlugins(DockerPlugin)

lazy val root = (project in file("."))
  .settings(
    name := "SoftwareDevelopmentProject"
  )

lazy val app = (project in file("Scala_project_2"))
  .settings(
    assembly / mainClass := Some("MainClass")
    // more settings here ...
  )

lazy val utils = (project in file("utils"))
  .settings(
    assembly / assemblyJarName := "SDT_summaries_app.jar"
    // more settings here ...
  )
//without this part we can't run sbt assembly
//assemblyMergeStrategy in assembly := {
//  case PathList("META-INF", _*) => MergeStrategy.discard
//  case _                        => MergeStrategy.first
//}

assemblyJarName in assembly := "SDT_summaries_app.jar"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "de.opal-project" % "tools_2.12" % "4.0.0",
  "de.opal-project" % "framework_2.12" % "4.0.0",
  "de.opal-project" % "common_2.12" % "4.0.0",
  "de.opal-project" % "static-analysis-infrastructure_2.12" % "4.0.0",
  "de.opal-project" % "bytecode-representation_2.12" % "4.0.0",
  "de.opal-project" % "abstract-interpretation-framework_2.12" % "4.0.0",
  "de.opal-project" % "three-address-code_2.12" % "4.0.0",
  "de.opal-project" % "architecture-validation_2.12" % "4.0.0",
  "com.databricks" %% "spark-xml" % "0.15.0",
//for scala test - flatSpec
  "org.scalatest" %% "scalatest" % "3.2.14" % "test",
//for transfering xml file to json one
  "org.json4s" %% "json4s-jackson" % "3.2.11",
  "io.spray" %% "spray-json" % "1.3.6"
)

unmanagedResourceDirectories in Compile += baseDirectory.value / "src" / "main" / "resources"
fullClasspath in Runtime += Attributed.blank(file("src/main/resources"))


ThisBuild / assemblyMergeStrategy := {
  case PathList("xml_as_json", xs@_*) => MergeStrategy.concat
  case x if Assembly.isConfigFile(x) => MergeStrategy.concat
  case PathList(ps @ _*) if Assembly.isReadme(ps.last) || Assembly.isLicenseFile(ps.last) =>
    MergeStrategy.rename
  case PathList("META-INF", xs @ _*) =>
    (xs map {_.toLowerCase}) match {
      case ("manifest.mf" :: Nil) | ("index.list" :: Nil) | ("dependencies" :: Nil) =>
        MergeStrategy.discard
      case ps @ (x :: xs) if ps.last.endsWith(".sf") || ps.last.endsWith(".dsa") =>
        MergeStrategy.discard
      case "plexus" :: xs =>
        MergeStrategy.discard
      case "services" :: xs =>
        MergeStrategy.filterDistinctLines
      case ("spring.schemas" :: Nil) | ("spring.handlers" :: Nil) =>
        MergeStrategy.filterDistinctLines
      case _ => MergeStrategy.last
    }
  case _ => MergeStrategy.last
}

//assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false, includeDependency = false, includeBin = true)
//assemblyMergeStrategy in assembly := {
//  case PathList("src","main","jar-files","classes.jar", xs @ _*) => MergeStrategy.discard
//  case x => MergeStrategy.defaultMergeStrategy(x)
//}
//
//assemblyMergeStrategy in assembly := {
//  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
//  case x => MergeStrategy.first
//}
//assemblyExcludedJars in assembly := {
//  val cp = (fullClasspath in assembly).value
//  cp filter {_.data.getName == "asm-3.3.1.jar"}
//}
//assemblyJarName in assembly := s"${name.value}-${version.value}.jar"
