enablePlugins(ScalaJSPlugin)

name := "Example"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.6"

persistLauncher in Compile := true

libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.8.0",
      "com.github.japgolly.scalajs-react" %%% "core" % "0.8.4"
)
jsDependencies += "org.webjars" % "react" % "0.12.1" / "react-with-addons.js" commonJSName "React"
