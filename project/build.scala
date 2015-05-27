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
    name := "widow_jane",
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

    val snowplowRawEvent = "com.snowplowanalytics" %  "snowplow-thrift-raw-event" % "0.1.0"
    val collectorPayload = "com.snowplowanalytics" %  "collector-payload-1"       % "0.0.0"
    val snowplowCollector = "com.snowplowanalytics" %% "snowplow-collector"       % "0.3.0.VWO"
    val snowplow = Seq(snowplowRawEvent, collectorPayload, snowplowCollector)

    val scalaz           = "org.scalaz"            %% "scalaz-core"               % "7.1.1"
    val apacheCommonsCodec = "commons-codec"       % "commons-codec"              % "1.10"
    val apacheCommonsMath = "org.apache.commons"   % "commons-math3"              % "3.5"

    val guava            = "com.google.guava"      % "guava"                      % "18.0"
    val spire            = "org.spire-math"        %% "spire"                     % "0.9.1"

    val akkaVersion             = "2.3.9"
    val akkaActor        = "com.typesafe.akka"     %% "akka-actor"                % akkaVersion
    val akkaSlf4j        = "com.typesafe.akka"     %% "akka-slf4j"                % akkaVersion
    val akkaStream       = "com.typesafe.akka"     %% "akka-stream-experimental"  % "1.0-M5"
    val akka = Seq(akkaActor, akkaSlf4j, akkaStream)

    val sprayVersion     = "1.3.2"
    val sprayCan         = "io.spray"              %% "spray-can"                 % sprayVersion
    val sprayRouting     = "io.spray"              %% "spray-routing"             % sprayVersion
    val sprayJson        = "io.spray"              %%  "spray-json"               % "1.3.1"
    val sprayCache       = "io.spray"              %% "spray-caching"              % sprayVersion
    val sprayJsonLenses  = "net.virtual-void"      %%  "json-lenses"              % "0.6.0"

    val spray = Seq(sprayCan, sprayRouting, sprayJson, sprayJsonLenses, sprayCache)

    val logback          = "ch.qos.logback"        %  "logback-classic"           % "1.0.13"

    val kafkaClients     = "org.apache.kafka"      %  "kafka-clients"             % "0.8.2.0"
    val kafkaCore        = "org.apache.kafka"      %%  "kafka"                    % "0.8.2.0" //We should make this go away once the client consumer works
    val kafka = Seq(kafkaClients, kafkaCore)

    val metrics          = "io.dropwizard.metrics" %  "metrics-core"              % "3.1.0"
    val metricsGraphite  = "io.dropwizard.metrics" %  "metrics-graphite"          % "3.1.0"
    val instrumentation = Seq(metrics, metricsGraphite)

    val thrift          = "org.apache.thrift"      %  "libthrift"                 % "0.9.1"  % "compile"

    val scalaCheck       = "org.scalacheck"        %% "scalacheck"                % "1.12.2" % "test"

    val all = Seq(scalaz, logback, apacheCommonsCodec, apacheCommonsMath, guava, spire, thrift, scalaCheck) ++ spray ++ akka ++ snowplow ++ kafka ++ instrumentation
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

  lazy val widowJane = Project("oldmonk", file("."), settings = commonSettings)
}
