val scala3Version = "3.2.2"



lazy val root = project
  .in(file("."))
  .settings(
    name := "learnscala3",
    version := "0.1.0-SNAPSHOT",
    excludeFilter := "*.sc",
    scalaVersion := scala3Version,
    mainClass := Some("learnscala3.MyApp"),
    
    //libraryDependencies += "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
    //libraryDependencies += "org.scala-lang" %% "scala3-library" % scalaVersion.value,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % Test,
    libraryDependencies += "commons-io" % "commons-io" % "2.11.0",
    libraryDependencies += "org.scala-lang" %% "scala3-tasty-inspector" % scalaVersion.value,
    libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.8.2"

  )



