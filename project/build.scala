import sbt._
import Defaults._
import Keys._
import sbtassembly.AssemblyPlugin.autoImport._

object ApplicationBuild extends Build {

  lazy val commonSettings = Defaults.defaultSettings ++ Seq(
    organization := "com.vwo",
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    scalaVersion := "2.11.5",
    version := "0.01." + "git rev-parse HEAD".!!.trim,
    resolvers ++= myResolvers,
    scalacOptions := Seq("-deprecation"),
    name := "oldmonk",
    //fork := true,
    libraryDependencies ++= Dependencies.all,
    // Akka settings
    //Assembly settings
    test in assembly := {},
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2"),
    unmanagedSourceDirectories in Compile += baseDirectory.value / "src" / "main" / "gen-java",
    mergeStrategy in assembly ~= {
      (old) => {
        {
          case x if x.startsWith("com/typesafe") => MergeStrategy.last
          case x if x == "reference.conf" => MergeStrategy.concat
          case x if x.endsWith("~") => MergeStrategy.discard
          case inf if inf.startsWith("META-INF/") =>
            inf.slice("META-INF/".size, inf.size).toLowerCase match {
              case "manifest.mf" | "index.list" | "dependencies" =>
                MergeStrategy.discard
              case n if n.endsWith(".sf") || n.endsWith(".dsa") =>
                MergeStrategy.discard
              case n if n startsWith "plexus/" =>
                MergeStrategy.discard
              case n if (n startsWith "notice") || (n startsWith "license") =>
                MergeStrategy.discard
              case n if n startsWith "services/" =>
                MergeStrategy.filterDistinctLines
              case "spring.schemas" | "spring.handlers" =>
                MergeStrategy.filterDistinctLines
              case _ => MergeStrategy.deduplicate
            }
          case n if n.contains("org/slf4j/impl/") =>
            MergeStrategy.first
          case x => MergeStrategy.deduplicate
        }
      }
    }
  )

  object Dependencies {
    val scalaz           = "org.scalaz"            %% "scalaz-core"               % "7.1.2"
    val guava            = "com.google.guava"      % "guava"                      % "18.0"
    val spire            = "org.spire-math"        %% "spire"                     % "0.9.1"

    val scalaCheck       = "org.scalacheck"        %% "scalacheck"                % "1.12.2" % "test"

    val all = Seq(scalaz, guava, spire, scalaCheck)
  }

  val myResolvers = Seq(
    "Sonatatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
    "Sonatatype Releases" at "http://oss.sonatype.org/content/repositories/releases",
    "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases",
    "Typesafe Snapshots" at "http://repo.typesafe.com/typesafe/snapshots",
    "Maven dot org" at "http://repo1.maven.org/maven2/",
    "Coda Hale" at "http://repo.codahale.com",
    "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
    "Snowplow Analytics Maven repo"          at "http://maven.snplow.com/releases/",
    "Snowplow Analytics Maven snapshot repo" at "http://maven.snplow.com/snapshots/",
    "Spray repo"                             at "http://repo.spray.io",
    "BintrayJCenter"                         at "http://jcenter.bintray.com", // For Scalazon
    "bigtoast-github"                        at "http://bigtoast.github.com/repo/" // For sbt-thrift
  )

  lazy val oldmonk = Project("oldmonk", file("."), settings = commonSettings)
}
