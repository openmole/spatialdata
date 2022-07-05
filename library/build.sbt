import sbt.Keys.publishMavenStyle
import sbt.enablePlugins

val geotoolsVersion = "23.0"

lazy val commonSettings = Seq(
  scalaVersion := "2.13.8",
  organization := "org.openmole.library",
  resolvers ++= Seq(
    "osgeo" at "https://repo.osgeo.org/repository/release/",
    "geosolutions" at "https://maven.geo-solutions.it/",
    "geotoolkit" at "https://maven.geotoolkit.org/"//,
    //"Boundless" at "https://repo.boundlessgeo.com/main"
    //"geonw" at "https://repo.osgeo.org/repository/geonetwork-releases/", // 20220618 - try for CI build to find jai_core; 20220704: why does CI not try other resolvers? test order change
    //"apache" at "https://repo.maven.apache.org/maven2",
    //Resolver.sonatypeRepo("snapshots"),
    //Resolver.sonatypeRepo("staging"),
    //Resolver.mavenCentral
  ),
  resolvers -= DefaultMavenRepository,
  useCoursier := false, // needed to have jai_core, otherwise empty in Coursier
  libraryDependencies ++= Seq(
    "org.apache.commons" % "commons-math3" % "3.6.1",
    "org.apache.commons" % "commons-lang3" % "3.1",
    "org.apache.commons" % "commons-rng-sampling" % "1.3",
    "org.apache.commons" % "commons-rng-simple" % "1.3",
    "net.sourceforge.jdistlib" % "jdistlib" % "0.4.5",
    "com.github.pathikrit" %% "better-files" % "3.8.0",
    "org.locationtech.jts" % "jts" % "1.16.1" pomOnly(),
    "org.geotools" % "gt-shapefile" % geotoolsVersion , //exclude("com.vividsolutions", "jts-core"), //exclude("javax.media", "jai_core")
    "org.geotools" % "gt-geopkg" % geotoolsVersion, // to read GeoPackage files (112k only)
    "org.geotools" % "gt-referencing" % geotoolsVersion,
    "org.geotools" % "gt-epsg-hsql" % geotoolsVersion, // explicitly load for CRS decoding - enough? EPSG:54009 not found
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
    "com.github.fommil.netlib" % "all" % "1.1.2", // impl for breeze
    "de.ruedigermoeller" % "fst" % "2.57",
    "org.openstreetmap.pbf" % "osmpbf" % "1.4.0",
    "javax.media" % "jai-core" % "1.1.3" from "https://repo.osgeo.org/repository/release/javax/media/jai_core/1.1.3/jai_core-1.1.3.jar" //20220704 test direct jar
    //"javax.media.jai" % "com.springsource.javax.media.jai.core" % "1.1.3"
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
//lazy val bundle = taskKey[Unit]("OSGI bundle")
//bundle := {
  OsgiKeys.exportPackage := Seq("org.openmole.spatialdata.application.*;-split-package:=merge-first")
  OsgiKeys.importPackage := Seq("*;resolution:=optional;-split-package:=merge-first")
  //OsgiKeys.privatePackage := Seq("!scala.*,!java.*,!monocle.*,!META-INF.*.RSA,!META-INF.*.SF,!META-INF.*.DSA,META-INF.services.*,META-INF.*,*")
OsgiKeys.privatePackage := Seq("!scala.*,!java.*,!monocle.*,!algebra.*,*")
OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""
//  (OsgiKeys.bundle in spatialdata).value
//}



/**
  * Assemble as a unique jar
  *  https://github.com/sbt/sbt-assembly#publishing-not-recommended
  */
//lazy val assemble = taskKey[Unit]("assemble")
lazy val assemblyMainClass = "org.openmole.spatialdata.test.Test"
//assemble := {
  assemblyMergeStrategy in assembly := {
    case PathList("META-INF", _@_*) => MergeStrategy.discard
    case _ => MergeStrategy.last
  }

  // ! scalaVersion not set: absurd file name; run assemble: no config (other key in sbt-assembly?)
  //assemblyJarName in assembly := "spatialdata-assembly_" + scalaVersion + ".jar"
  assemblyJarName in assembly := "spatialdata-assembly_2.13.jar"

  artifact in(Compile, assembly) := {
    val art = (artifact in(Compile, assembly)).value
    art.withClassifier(Some("assembly"))
  }

  addArtifact(artifact in(Compile, assembly), assembly)

  mainClass in (Compile, packageBin) := Some(assemblyMainClass)

//  (assembly in spatialdata).value
//}


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

