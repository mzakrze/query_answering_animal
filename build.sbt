name := "sag_animals_knowledge"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.19",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.19" % Test,
  "com.typesafe.akka" %% "akka-http"   % "10.1.7",
  "org.scalatest" %% "scalatest" % "3.0.5" % Test
)

libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "2.1.0"
libraryDependencies += "org.jsoup" % "jsoup" % "1.9.1"
libraryDependencies += "com.google.code.gson" % "gson" % "2.8.5"
libraryDependencies += "org.apache.opennlp" % "opennlp-tools" % "1.9.0"
libraryDependencies += "org.scalaj" %% "scalaj-http" % "2.4.1"
libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.5.19"