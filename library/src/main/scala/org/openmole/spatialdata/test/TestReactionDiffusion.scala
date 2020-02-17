package org.openmole.spatialdata.test

import org.apache.commons.math3.stat.inference.KolmogorovSmirnovTest
import org.openmole.spatialdata.grid.Implicits._
import org.openmole.spatialdata.application.reactiondiffusion.ReactionDiffusionCalibration
import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.measures.GridMorphology.{AverageDistance, Entropy, Moran, Slope}
import org.openmole.spatialdata.grid.real.CSVGridGenerator
import org.openmole.spatialdata.grid.synthetic.ReactionDiffusionGridGenerator
import org.openmole.spatialdata.utils.io.CSV
import org.openmole.spatialdata.utils.{math, visualization, withTimer}

import scala.util.Random

object TestReactionDiffusion {


  def testRealDataCalibration(): Unit = {

    val rng = new Random

    val params = (0 until 50000).map{_ =>
      (5.0*rng.nextDouble(),0.1*rng.nextDouble(),10.0+90*rng.nextDouble(),rng.nextLong())
    }

    val targetArea = 100
    val initYear = "1990"
    val targetYear = "2000"

    val initialConfig = CSVGridGenerator("data/ucdb/"+targetArea+"_"+initYear+".csv",";").generateGrid(rng).map{_.toSeq}.toSeq
    val finalConfig = CSVGridGenerator("data/ucdb/"+targetArea+"_"+targetYear+".csv",";").generateGrid(rng).map{_.toSeq}.toSeq

    val objs = CSV.readCSV("data/ucdb/morphologies.csv",sep=";")
    val targetPop = objs("totalPop"+targetYear)(targetArea-1).toDouble
    val targetMoran = objs("moran"+targetYear)(targetArea-1).toDouble
    val targetAvgDist = objs("avgDist"+targetYear)(targetArea-1).toDouble
    val targetEntropy = objs("entropy"+targetYear)(targetArea-1).toDouble
    val targetSlope = objs("alpha"+targetYear)(targetArea-1).toDouble

    val res = params.map {case (alpha,beta,tsteps,s) =>
      val calib = ReactionDiffusionCalibration(initialConfig,finalConfig,alpha,beta,1.0,tsteps,targetPop,targetMoran,targetAvgDist,targetEntropy,targetSlope,s)
      val (r,t) = withTimer[Unit,Double]{_ => calib.runModel._1}()
      println(t)
      (calib,r,t)
    }

    val best = res.minBy(_._2)(Ordering.Double.TotalOrdering)

    println(s"Best is err = ${best._2}")
    println(s"Avg time = ${res.map{_._3}.sum/res.length}")

  }


  def testModel(): Unit = {

    implicit val rng = new Random

    val targetArea = 100
    val initYear = 1975
    val targetYear = 1990
    //val targetYear = 2015

    //

    val initialConfig = CSVGridGenerator("data/ucdb/"+targetArea+"_"+initYear+".csv",";").generateGrid(rng)
    val finalConfig = CSVGridGenerator("data/ucdb/"+targetArea+"_"+targetYear+".csv",";").generateGrid(rng)

    //println(initialConfig)
    visualization.staticRasterVisualization(initialConfig.map{_.map{d => if(d.isNaN) 0.0 else d}})

    val deltaPop = finalConfig.flatten.filter(!_.isNaN).sum - initialConfig.flatten.filter(!_.isNaN).sum

    //1.2819520088589078,0.2938849030874698,27.432594956849854
    //0.2216093312569525,1.3820604067624241E-4,6.30040094455728,1.4048088387268383,9
    // 1.046268668726121,0.0011831875088893903,97.987991963447,2.4225641231727895E8,51

    //val generatoriter = ReactionDiffusionGridGenerator(100,100,1000,0.5,0.8,1)
    //val generator = ReactionDiffusionGridGenerator(size =(56,62),alpha = 8.844, beta = 0.09763,growthRate = 2033, totalPopulation = 189215, diffusionSteps = 1)
    val generator = ReactionDiffusionGridGenerator(size =(56,62),alpha = 1.04626, beta = 0.001183,growthRate = (deltaPop / 97).toInt,
      totalPopulation = finalConfig.flatten.filter(!_.isNaN).sum.toInt, diffusionSteps = 1,initialConfiguration = Some(initialConfig.map{_.toSeq}.toSeq))

    val grid = generator.generateGrid.map{_.map{d => if(d.isNaN) 0.0 else d}}

    println(deltaPop)
    println(math.relSquare(finalConfig.flatten.filter(!_.isNaN).sum,grid.flatten.sum))

    visualization.staticRasterVisualization(grid)
    visualization.staticRasterVisualization(finalConfig.map{_.map{d => if(d.isNaN) 0.0 else d}})

    visualization.staticRasterVisualization(initialConfig.zip(grid).map{case (r1,r2) => r1.zip(r2).map{case (p1,p2) => p2-p1}.map{d => if (d.isNaN) 0.0 else d}})
    visualization.staticRasterVisualization(initialConfig.zip(finalConfig).map{case (r1,r2) => r1.zip(r2).map{case (p1,p2) => p2-p1}.map{d => if (d.isNaN) 0.0 else d}})

  }


  def benchmarkImplementation(): Unit ={

    implicit val rng = new Random

    // 189215.2320944653 ; alpha = 8.844474802962576 ; beta = 0.09763924370738188 ; tsteps = 93.04388345412039 ; growthRate = 2033.6127972105405

    //val generatoriter = ReactionDiffusionGridGenerator(100,100,10000,2.0,0.05,1)
    //val generator = ReactionDiffusionGridGenerator(100,100,10000,2.0,0.05,1,iterImpl = false)
    val generatoriter = ReactionDiffusionGridGenerator(size =(56,62),alpha = 8.844, beta = 0.09763,growthRate = 2033, totalPopulation = 189215, diffusionSteps = 1)
    val generator = generatoriter.copy(iterImpl = true)

    val res = (0 until 10).map { k =>
      println(k)
      val (morph1, t) = withTimer[Unit, GridMorphology] { _ =>
        GridMorphology(generatoriter.generateGrid,Seq(Moran(),AverageDistance(),Entropy(),Slope()))
      }()
      val (morph2, t2) = withTimer[Unit, GridMorphology] { _ =>
        GridMorphology(generator.generateGrid,Seq(Moran(),AverageDistance(),Entropy(),Slope()))
      }()
      (t,t2,morph1,morph2)
    }

    println("Iterative impl : "+res.map(_._1).sum / res.size)
    println("Direct impl : "+res.map(_._2).sum / res.size)

    // 837.52 ; 741.4
    // time gain is marginal: 0.1s
    // would make a difference for larger growth rates? but then much less steps
    // -> keep the standard implementation

    // check if stat distribs of indics are the same
    val kstest = new KolmogorovSmirnovTest
    println(kstest.kolmogorovSmirnovTest(res.map(_._3.moran).toArray,res.map(_._3.moran).toArray))
    //println(kstest.kolmogorovSmirnovTest(res.map(_._3.avgDistance).toArray,res.map(_._3.avgDistance).toArray))



  }


}
