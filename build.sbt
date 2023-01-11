ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.12.15"

lazy val root = (project in file("."))
  .settings(
    name := "SoftwareDevelopmentProject"
  )

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
libraryDependencies += "de.opal-project" % "tools_2.12" % "4.0.0"
libraryDependencies += "de.opal-project" % "framework_2.12" % "4.0.0"
libraryDependencies += "de.opal-project" % "common_2.12" % "4.0.0"
libraryDependencies += "de.opal-project" % "static-analysis-infrastructure_2.12" % "4.0.0"
libraryDependencies += "de.opal-project" % "bytecode-representation_2.12" % "4.0.0"
libraryDependencies += "de.opal-project" % "abstract-interpretation-framework_2.12" % "4.0.0"
libraryDependencies += "de.opal-project" % "three-address-code_2.12" % "4.0.0"
libraryDependencies += "de.opal-project" % "architecture-validation_2.12" % "4.0.0"
libraryDependencies += "com.databricks" %% "spark-xml" % "0.15.0"
//for scala test - flatSpec
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % "test"
