import scala.sys.process._
import scala.language.postfixOps

import sbtwelcome._

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val chessfronttyrian =
  (project in file("."))
    .enablePlugins(ScalaJSPlugin)
    .settings( // Normal settings
      name         := "chessfronttyrian",
      version      := "0.0.1",
      scalaVersion := "3.1.2",
      organization := "myorg",
      libraryDependencies ++= Seq(
        "io.indigoengine" %%% "tyrian-io" % "0.5.1",
        "org.scalameta"   %%% "munit"     % "0.7.29" % Test
      ),
      testFrameworks += new TestFramework("munit.Framework"),
      scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
      autoAPIMappings := true
    )
    .settings( // Welcome message
      logo := "ChessFrontTyrian (v" + version.value + ")",
      usefulTasks := Seq(
        UsefulTask("", "fastOptJS", "Rebuild the JS (use during development)"),
        UsefulTask(
          "",
          "fullOptJS",
          "Rebuild the JS and optimise (use in production)"
        ),
        UsefulTask("", "code", "Launch VSCode")
      ),
      logoColor        := scala.Console.MAGENTA,
      aliasColor       := scala.Console.BLUE,
      commandColor     := scala.Console.CYAN,
      descriptionColor := scala.Console.WHITE,
      libraryDependencies ++= Seq(
        "io.circe"      %%% s"circe-core"   % "0.14.2",
        "io.circe"      %%% s"circe-parser" % "0.14.2",
        "org.typelevel" %%% "kittens"       % "3.0.0-M4",
        "org.typelevel" %%% "cats-core"     % "2.7.0",
        "org.typelevel" %%% "cats-effect"   % "3.3.5",
        "org.scalatest" %%% "scalatest"     % "3.2.9"
      )
    )
