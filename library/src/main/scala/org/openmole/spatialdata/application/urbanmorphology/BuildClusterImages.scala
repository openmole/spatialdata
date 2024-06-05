package org.openmole.spatialdata.application.urbanmorphology

import java.io.File
import com.github.tototoshi.csv.CSVReader
import com.github.tototoshi.csv.defaultCSVFormat
import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.real.OSMGridGenerator
import org.openmole.spatialdata.utils.io.PNG
import org.openmole.spatialdata.utils.osm.APIExtractor.{OSMAPIMode, OSMOverpass}

import scala.io.Source
import scala.util.Random

object BuildClusterImages extends App {
  def sqDistance(p1: (Double, Double), p2: (Double, Double)) = Math.pow(p1._1 - p2._1, 2) + Math.pow(p1._2 - p2._2, 2)
  def buildClusterImagesReal(file: File,
                             outputDir: File,
                             clusters: Map[Int, (Double, Double)],
                             worldWidth: Int = 500,
                             mode: OSMAPIMode = OSMOverpass
                            )(implicit rng: Random): Unit = {
    val reader = CSVReader.open(file)
    val valuesWithDistance = reader.allWithHeaders().map {
      line => {
        val lon = line("lon").toDouble
        val lat = line("lat").toDouble
//        val height = line("height").toInt
        val width = line("width").toInt
        val cluster = line("cluster").toInt
        val pc1 = line("PC1").toDouble
        val pc2 = line("PC2").toDouble
        val distanceToCluster = sqDistance((pc1, pc2), clusters(cluster))
        (lon, lat, width, cluster, distanceToCluster)
      }
    }
    clusters.foreach {
      case (cluster, _) =>
        val values = valuesWithDistance.filter(_._4 == cluster).sortBy(_._5)(Ordering.Double.TotalOrdering).take(10)
        val directory = new File(outputDir.getAbsolutePath+"/"+cluster.toString)
        directory.mkdirs()
        values.foreach{
          case (lon, lat, width, _, distanceToCluster) =>
          val grid = OSMGridGenerator(lon,lat,worldWidth,width,mode).generateGrid
          PNG.write(grid, new File(directory.getAbsolutePath+s"/osm_${lon}_${lat}_$distanceToCluster.png"))
        }
    }
    reader.close()
  }
  def buildClusterImagesGenerators(file: File, outputDir: File, clusters: Map[Int, (Double, Double)], rotationFile: File, normFile: File)(implicit rng: Random): Unit =  {
    val reader = CSVReader.open(file)
    val rotation = Source.fromFile(rotationFile).getLines().toArray.map{_.split(",").map{_.toDouble}}
    val normalization = Source.fromFile(normFile).getLines().toArray.map{_.split(",").map{_.toDouble}}
    def projection(morphology: GridMorphology): Array[Double] = GridMorphology.rotation(rotation,normalization)(morphology)
    val valuesWithDistance = reader.toStreamWithHeaders.map {
      line => {
        val height = line("height").toDouble
        val width = line("width").toDouble
        val area = line("area").toDouble
        val moran = line("moran").toDouble
        val avgDistance = line("avgDistance").toDouble
        val density = line("density").toDouble
        val components = line("components").toDouble
        val avgDetour = line("avgDetour").toDouble
        val avgBlockArea = line("avgBlockArea").toDouble
        val avgComponentArea = line("avgComponentArea").toDouble
        val fullDilationSteps = line("fullDilationSteps").toDouble
        val fullErosionSteps = line("fullErosionSteps").toDouble
        val fullClosingSteps = line("fullClosingSteps").toDouble
        val fullOpeningSteps = line("fullOpeningSteps").toDouble

        val generator = line("generator")
        val replication = line("replication").toLong
        val size = line("size").toInt
        val randomDensity = line("randomDensity").toDouble
        val blocksMaxSize = line("blocksMaxSize").toDouble.toInt
        val blocksMinSize = line("blocksMinSize").toDouble.toInt
        val blocksNumber = line("blocksNumber").toDouble.toInt
        val expMixtureCenters = line("expMixtureCenters").toDouble.toInt
        val expMixtureRadius = line("expMixtureRadius").toDouble
        val expMixtureThreshold = line("expMixtureThreshold").toDouble
        val percolationBordPoints = line("percolationBordPoints").toDouble.toInt
        val percolationLinkWidth = line("percolationLinkWidth").toDouble
        val percolationProba = line("percolationProba").toDouble

        val proj = projection(GridMorphology(height,width,area,moran,avgDistance,0.0,(0.0,0.0),density,components,avgDetour,avgBlockArea,avgComponentArea,fullDilationSteps,fullErosionSteps,fullClosingSteps,fullOpeningSteps))
        val (cluster, distanceToCluster) = clusters.map{case (c, (pc1, pc2)) => (c, sqDistance((proj(0), proj(1)), (pc1, pc2)))}.minBy(_._2)(Ordering.Double.TotalOrdering)
        println(s"distance to cluster $cluster = $distanceToCluster")
        (cluster, distanceToCluster, width, height, size, replication, generator, randomDensity, blocksNumber, blocksMinSize, blocksMaxSize, expMixtureCenters, expMixtureRadius, expMixtureThreshold, percolationBordPoints, percolationLinkWidth, percolationProba)
      }
    }
    clusters.foreach {
      case (cluster, _) =>
        val values = valuesWithDistance.filter(_._1 == cluster).sortBy(_._2)(Ordering.Double.TotalOrdering).take(10)
        val directory = new File(outputDir.getAbsolutePath+"/"+cluster.toString)
        directory.mkdirs()
        values.foreach{
          case (_, distanceToCluster, _, _, size, replication, generator, randomDensity, blocksNumber, blocksMinSize, blocksMaxSize, expMixtureCenters, expMixtureRadius, expMixtureThreshold, percolationBordPoints, percolationLinkWidth, percolationProba) =>
            rng.setSeed(replication)
            val grid = GridGeneratorLauncher(generator, size, randomDensity, expMixtureCenters, expMixtureRadius, expMixtureThreshold, blocksNumber, blocksMinSize, blocksMaxSize, percolationProba, percolationBordPoints, percolationLinkWidth).getGrid
            PNG.write(grid, new File(directory.getAbsolutePath+s"/${generator}_$distanceToCluster.png"))
        }
    }
    reader.close()
  }
  implicit val rng: Random = new Random
  val clusters = Seq(
    (1, (-0.401568005874794, -0.139034657366536)),
    (2, (0.307899227735927, -0.357540683059979)),
    (3, (-0.00169022591544116, -0.0933680483743268)),
    (4, (-0.773530217615314, -0.294339386091329))).toMap

  //  buildClusterImagesReal(File("UrbanForm") / "realpoints.csv", File("UrbanForm") / "osm", clusters)
  buildClusterImagesGenerators(new File("UrbanForm/20190311_181654_LHS_GRID.csv"), new File("UrbanForm/generators"), clusters, new File("openmole/setup/pca.csv"), new File("openmole/setup/norm.csv"))
}
