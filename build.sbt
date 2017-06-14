lazy val root = (project in file(".")).
  settings(
    name := "scala99",
    version := "1.0.0",
    scalaVersion := "2.11.8"
  )

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.typelevel" %% "cats" % "0.9.0"
)