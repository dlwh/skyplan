import AssemblyKeys._ // put this at the top of the file

name := "skyplan"

version := "0.1"

organization := "dlwh"

scalaVersion := "2.10.0"

resolvers ++= Seq(
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
"Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.5" % "test",
  "org.scalanlp" %% "breeze-core" % "0.2-SNAPSHOT",
  "org.scalanlp" %% "breeze-math" % "0.2-SNAPSHOT",
  "org.scalanlp" %% "breeze-process" % "0.2-SNAPSHOT"
)

libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
  sv match {
    case "2.9.2" =>
      (deps :+ ("org.scalatest" % "scalatest" % "1.4.RC2" % "test"))
    case x if x.startsWith("2.8") =>
      (deps :+ ("org.scalatest" % "scalatest" % "1.3" % "test")
            :+ ("org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" % "test"))
    case _       =>
     (deps :+ ("org.scalacheck" %% "scalacheck" % "1.10.0" % "test")
           :+ ("org.scalatest" %% "scalatest" % "2.0.M5b" % "test"))
  }
}

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

javacOptions ++= Seq("-source", "1.5", "-target", "1.5")

scalacOptions ++= Seq("-deprecation")

javaOptions += "-Xmx2g"

// fork in run := true

// javaOptions in run += "-Xrunhprof:cpu=samples,depth=16"


seq(assemblySettings: _*)
