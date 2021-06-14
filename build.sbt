name := "scala"

version := "0.1"

scalaVersion := "2.12.7"

libraryDependencies ++= Seq(
  "org.tukaani" % "xz" % "1.6",
  "org.apache.commons" % "commons-compress" % "1.18",
  "org.apache.ant" % "ant" % "1.10.5",
  "org.typelevel" %% "cats-core" % "2.0.0"
)