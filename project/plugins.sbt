resolvers ++= Seq("Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/")

resolvers += Resolver.url("artifactory", url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

resolvers += Resolver.url("sbt-plugin-snapshots", url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-snapshots"))(Resolver.ivyStylePatterns)

resolvers ++= Seq("antelink-public-jboss-mirror" at "http://maven.antelink.com/content/groups/public-jboss/")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.13.0")

logLevel := Level.Warn

resolvers += "Scala Tools Releases" at "https://repository.jboss.org/nexus/content/repositories/scala-tools-releases"

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "0.8.0-M2")
