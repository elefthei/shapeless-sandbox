name := "generics-sandbox"

version := "0.1"

scalaVersion := "2.13.0"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3"
)
libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0-M4"
libraryDependencies += "org.typelevel" %% "cats-macros" % "2.0.0-M4"
libraryDependencies += "org.typelevel" %% "cats-kernel" % "2.0.0-M4"

