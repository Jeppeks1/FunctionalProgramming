
name := "Scala"

version := "0.1"

scalaVersion := "2.12.5"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

logBuffered in Test := false

val scalazVersion = "7.2.21"

libraryDependencies ++= Seq (
  "org.scalaz" %% "scalaz-core"               % scalazVersion,
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test" )

val monocleVersion = "1.5.0"

libraryDependencies ++= Seq(
  "com.github.julien-truffaut" %%  "monocle-core"    % monocleVersion,
  "com.github.julien-truffaut" %%  "monocle-generic" % monocleVersion,
  "com.github.julien-truffaut" %%  "monocle-macro"   % monocleVersion,
  "com.github.julien-truffaut" %%  "monocle-state"   % monocleVersion,
  "com.github.julien-truffaut" %%  "monocle-law"     % monocleVersion % "test"
)


