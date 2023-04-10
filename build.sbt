ThisBuild / version := "0.1"

ThisBuild / scalaVersion := "2.13.9"

lazy val root = (project in file("."))
  .settings(
    name := "scala",
    libraryDependencies ++= Seq(
      "org.tukaani" % "xz" % "1.9",
      "org.apache.commons" % "commons-compress" % "1.21",
      "org.apache.ant" % "ant" % "1.10.13",
      "org.typelevel" %% "cats-core" % "2.9.0",
      "org.typelevel" %% "cats-effect" % "3.4.6",
      "io.circe" %% "circe-core" % "0.14.4",
      "io.circe" %% "circe-generic" % "0.14.4",
      "io.circe" %% "circe-generic-extras" % "0.14.3",
      "io.circe" %% "circe-parser" % "0.14.4"
    )
  )