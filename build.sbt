import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "design.sandwwraith.mlhw",
      scalaVersion := "2.12.4",
      version := "0.1.0-SNAPSHOT"
    )),
    name := "MathlogHW",
    libraryDependencies += parbolied,
    libraryDependencies += scalaTest % Test
  )
