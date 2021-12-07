name := "refreshCatsEffect"

version := "0.1"

scalaVersion := "2.13.7"

libraryDependencies ++= Seq {

  val catsEffectVersion = "3.2.8"

  "org.typelevel" %% "cats-effect" % catsEffectVersion
}
