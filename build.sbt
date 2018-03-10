import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Hello",
    libraryDependencies += scalaTest % Test
      libraryDependencies ++= Seq("org.scalanlp" %% "breeze" % "0.13.2",
        "org.scalanlp" %% "breeze-natives" % "0.13.2",
        "org.scalanlp" %% "breeze-viz" % "0.13.2"
      )
      resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
  )
