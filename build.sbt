ThisBuild / version := "0.1"

ThisBuild / scalaVersion := "2.13.9"

lazy val root = (project in file("."))
  .settings(
    name := "scala",
    libraryDependencies ++= Seq(
      "org.tukaani"        % "xz"                   % "1.9",
      "org.apache.commons" % "commons-compress"     % "1.21",
      "org.apache.ant"     % "ant"                  % "1.10.14",
      "org.typelevel"     %% "cats-core"            % "2.10.0",
      "org.typelevel"     %% "cats-effect"          % "3.4.6",
      "io.circe"          %% "circe-core"           % "0.14.6",
      "io.circe"          %% "circe-generic"        % "0.14.6",
      "io.circe"          %% "circe-generic-extras" % "0.14.3",
      "io.circe"          %% "circe-parser"         % "0.14.6",
      "org.typelevel"     %% "cats-core"            % "2.10.0",
      "org.scalatest"     %% "scalatest"            % "3.2.17" % Test,
      "org.scalacheck"    %% "scalacheck"           % "1.17.0" % Test
    )
  )
