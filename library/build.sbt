import sbt.Keys.publishMavenStyle
import sbt.enablePlugins

val geotoolsVersion = "23.0"

lazy val commonSettings = Seq(
  scalaVersion := "2.13.8",
  organization := "org.openmole.library",
  // trick to remove maven central (added again: change order for jai to be found)
  externalResolvers := Resolver.combineDefaultResolvers(resolvers.value.toVector, mavenCentral = false),
  resolvers ++= Seq(
    "osgeo" at "https://repo.osgeo.org/repository/release/",
    "geosolutions" at "https://maven.geo-solutions.it/",
    "geotoolkit" at "https://maven.geotoolkit.org/",
    //"Boundless" at "https://repo.boundlessgeo.com/main"
    //"geonw" at "https://repo.osgeo.org/repository/geonetwork-releases/", // 20220618 - try for CI build to find jai_core; 20220704: why does CI not try other resolvers? test order change
    //"apache" at "https://repo.maven.apache.org/maven2",
    //Resolver.sonatypeRepo("snapshots"),
    //Resolver.sonatypeRepo("staging"),
    Resolver.mavenCentral
  ),
  useCoursier := false, // needed to have jai_core, otherwise empty in Coursier
  libraryDependencies ++= Seq(
    "org.apache.commons" % "commons-math3" % "3.6.1",
    "org.apache.commons" % "commons-lang3" % "3.1",
    "org.apache.commons" % "commons-rng-sampling" % "1.3",
    "org.apache.commons" % "commons-rng-simple" % "1.3",
    "net.sourceforge.jdistlib" % "jdistlib" % "0.4.5",
    "com.github.pathikrit" %% "better-files" % "3.8.0",
    //"org.locationtech.jts" % "jts" % "1.16.1" pomOnly(),
    "org.geotools" % "gt-shapefile" % geotoolsVersion ,
    "org.geotools" % "gt-geopkg" % geotoolsVersion,
    "org.geotools" % "gt-referencing" % geotoolsVersion,
    "org.geotools" % "gt-epsg-hsql" % geotoolsVersion,
    "org.geotools" % "gt-epsg-extension" % geotoolsVersion,
    "org.geotools" % "gt-geotiff" % geotoolsVersion,
    "org.geotools" % "gt-coverage" % geotoolsVersion,
    "org.geotools" % "gt-opengis" % geotoolsVersion,
    "com.github.tototoshi" %% "scala-csv" % "1.3.6",
    "org.postgresql" % "postgresql" % "42.2.5",
    "org.mongodb" % "mongo-java-driver" % "3.10.0",
    "org.jgrapht" % "jgrapht-core" % "1.3.1",
    "org.apache.httpcomponents" % "httpclient" % "4.3.5",
    "commons-io" % "commons-io" % "2.3",
    "org.scalanlp" %% "breeze" % "1.0",
    "com.github.fommil.netlib" % "all" % "1.1.2",
    "de.ruedigermoeller" % "fst" % "2.57",
    "org.openstreetmap.pbf" % "osmpbf" % "1.4.0",
    "javax.media" % "jai-core" % "1.1.3" from "https://repo.osgeo.org/repository/release/javax/media/jai_core/1.1.3/jai_core-1.1.3.jar"
  ),
  cancelable in Global := true,
  scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation","-feature")
)


lazy val overwriteNonSnapshot = true // use to overwrite when publish non-snapshot if issue during a previous release tentative
lazy val publishSettings = Seq(
  useGpg := true,
  publishMavenStyle in ThisBuild := true,
  publishTo in ThisBuild := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
    else Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  publishConfiguration := publishConfiguration.value.withOverwrite(overwriteNonSnapshot),
  //credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
  licenses in ThisBuild := Seq("Affero GPLv3" -> url("http://www.gnu.org/licenses/")),
  homepage in ThisBuild := Some(url("https://github.com/openmole/spatialdata")),
  scmInfo in ThisBuild := Some(ScmInfo(url("https://github.com/openmole/spatialdata.git"), "scm:git:git@github.com:openmole/spatialdata.git")),
  pomExtra in ThisBuild :=
  <developers>
    <developer>
      <id>justeraimbault</id>
      <name>Juste Raimbault</name>
    </developer>
    <developer>
      <id>julienperret</id>
      <name>Julien Perret</name>
    </developer>
  </developers>
)



lazy val spatialdata = project.in(file(".")).settings(
  Seq(name := "spatialdata") ++ commonSettings ++ publishSettings
)


/**
  * Testing with scalatest
  */
lazy val test = project.in(file("test")) dependsOn spatialdata settings(
  commonSettings ++ Seq(
    name := "test",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0"
  )
)

/**
  * OSGI bundle
  */
enablePlugins(SbtOsgi)

OsgiKeys.exportPackage := Seq("org.openmole.spatialdata.application.*;-split-package:=merge-first")
OsgiKeys.importPackage := Seq("*;resolution:=optional")
OsgiKeys.privatePackage := Seq("!java.*,!scala.*,*")
OsgiKeys.requireCapability := """osgi.ee; osgi.ee="JavaSE";version:List="1.8,1.9""""
// include geotools jars to have service files for factories
OsgiKeys.embeddedJars := (Compile / Keys.externalDependencyClasspath).value map (_.data) filter (f=> f.getName startsWith "gt-")


lazy val runtest = taskKey[Unit]("run test main class")
runtest := {
  mainClass in (Compile,run) := Some("org.openmole.spatialdata.test.Test")
  fork in run := true
}


/**
  * Releasing: run releaseConfig, release
  */
lazy val releaseConfig = taskKey[Unit]("Release configuration")
releaseConfig := {
  sonatypeProfileName := "org.openmole"

  import sbtrelease.ReleasePlugin.autoImport.ReleaseTransformations._

  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    setReleaseVersion,
    tagRelease,
    releaseStepCommand("publishSigned"),
    //setNextVersion,
    //commitNextVersion,
    releaseStepCommand("sonatypeRelease")
    //releaseStepCommand("sonatypeReleaseAll")//,
    //pushChanges
  )
}

