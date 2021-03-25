
package org.openmole.spatialdata.grid.measures

import org.apache.commons.math3.linear.MatrixUtils
import org.apache.commons.math3.util.MathArrays

import org.openmole.spatialdata._
import org.openmole.spatialdata.grid._
import org.openmole.spatialdata.network._
import org.openmole.spatialdata.utils.graph.GraphAlgorithms
import org.openmole.spatialdata.utils.io.CSV
import org.openmole.spatialdata.utils.math._
import org.openmole.spatialdata.utils.Implicits._

import scala.util.Random


/**
  *
  * check https://github.com/martinfleis/momepy for more building level morphology measures
  * Fleischmann, (2019). momepy: Urban Morphology Measuring Toolkit. Journal of Open Source Software, 4(43), 1807, https://doi.org/10.21105/joss.01807
  *
  * check https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0225734 for landscape ecology metrics
  *
  * check Nowosad, J. (2021). Motif: an open-source R tool for pattern-based spatial analysis. Landscape Ecology, 36(1), 29-43. for spatial pattern
  *
  *  // interdisciplinary comparison of approaches / metrics?
  *
  * @param height height
  * @param width width
  * @param area area
  * @param moran moran
  * @param avgDistance average distance
  * @param entropy entropy
  * @param slope slope
  * @param density density
  * @param components number of connected components
  * @param avgDetour average detour in underlying graph
  * @param avgBlockArea average block area
  * @param avgComponentArea average component area
  * @param fullDilationSteps dilation steps
  * @param fullErosionSteps erosion steps
  * @param fullClosingSteps closing steps
  * @param fullOpeningSteps opening steps
  */
case class GridMorphology(
                       height: Double,
                       width: Double,
                       area: Double,
                       moran: Double,
                       avgDistance: Double,
                       entropy: Double,
                       slope: (Double,Double),
                       density: Double,
                       components: Double,
                       avgDetour: Double,
                       avgBlockArea: Double,
                       avgComponentArea: Double,
                       fullDilationSteps: Double,
                       fullErosionSteps: Double,
                       fullClosingSteps: Double,
                       fullOpeningSteps: Double
                     ) {

  def toTuple: (Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double,Double) =
    (height,width,area,moran,avgDistance,density,components,avgDetour,avgBlockArea,avgComponentArea,fullDilationSteps,fullErosionSteps,fullClosingSteps,fullOpeningSteps)

  def toArray(n: Int = -1): Array[Double] = {
    n match {
      case -1 => toTuple.productIterator.toArray.map{_.asInstanceOf[Double]}
      case 0 => Array.empty
      case nn: Int if nn > 0 => toTuple.productIterator.toArray.map{_.asInstanceOf[Double]}.takeRight(nn+3).take(nn)
    }
  }

  def |-|(m2: GridMorphology): Double = toArray().zip(m2.toArray()).map{case (o1,o2) => scala.math.abs(o1-o2)}.sum

}


/**
  * Morphological indicators for spatial density grids
  *
  * (see Raimbault, J. (2018). Calibration of a density-based model of urban morphogenesis. PloS one, 13(9), e0203516.)
  *
  * Future work
  *    - See https://github.com/locationtech/geotrellis if some are implemented in raster operations
  *    - to finish -> separate block based indicators from density grid ?
  */
object GridMorphology {


  sealed trait GridMorphologyIndicator
  case class Moran() extends GridMorphologyIndicator
  case class AverageDistance() extends GridMorphologyIndicator
  case class Entropy() extends GridMorphologyIndicator
  case class Slope() extends GridMorphologyIndicator

  /**
    * Compute given indicators
    * @param grid grid
    * @param indicators indicator list
    * @return
    */
  def apply(grid: RasterLayerData[Double], indicators: Seq[GridMorphologyIndicator]): GridMorphology = {
    GridMorphology(grid.length,grid(0).length,grid.flatten.sum,
      if (indicators.contains(Moran())) moran(grid) else 0.0,
      if (indicators.contains(AverageDistance())) distanceMean(grid) else 0.0,
      if (indicators.contains(Entropy())) Statistics.entropy(grid) else 0.0,
      if (indicators.contains(Slope())) Statistics.slope(grid) else (0.0,0.0),
      density(grid),
      Double.NaN,Double.NaN,Double.NaN,Double.NaN,Double.NaN,Double.NaN,Double.NaN,Double.NaN
    )
  }


  /**
    * Compute morphological indicators for a given grid
    *
    *  - construct a specific random here? NO implicit
    *  - rq: opening and closing are interesting as profile of mask radius (always one or two with the smaller mask); not computed: too complicated/costly to compute
    * @param grid grid
    * @return
    */
  def apply(grid: RasterLayerData[Double])(implicit rng: Random): GridMorphology = {
    val cachedNetwork = network.gridToNetwork(grid)
    GridMorphology(
      grid.length,grid(0).length,
      grid.flatten.sum,
      moranDirect(grid),
      distanceMeanDirect(grid),
      Double.NaN,(Double.NaN,Double.NaN),
      density(grid),
      components(grid,Some(cachedNetwork)),
      avgDetour(grid,Some(cachedNetwork)),
      avgBlockArea(grid,Some(cachedNetwork)),
      avgComponentArea(grid),
      fullDilationSteps(grid),
      fullErosionSteps(grid),
      Double.NaN,
      Double.NaN
    )
  }

  /**
    * read a rotation from file and perform it on the normalized corresponding components
    * (can be projected if not same number of arrows as number of elements in Morphology)
    * @param rotation rotation
    * @param normalization normalization
    * @param morpho morphological indicators
    * @return
    */
  def rotation(rotation: Array[Array[Double]],normalization: Array[Array[Double]])(morpho: GridMorphology): Array[Double] = {
    val rot = MatrixUtils.createRealMatrix(rotation)
    val n = rot.getRowDimension
    val morphonorm = morpho.toArray(n).zip(normalization).map{case (m,a) => (a(0)-m)/(a(0)-a(1))}
    //println("normalized : "+morphonorm.toSeq)
    val rotated = MatrixUtils.createRowRealMatrix(morphonorm).multiply(rot).getRow(0)
    //println("rotated : "+rotated.toSeq)
    rotated
  }

  def rotation(rotFile: String,normFile: String)(morpho: GridMorphology): Array[Double] = rotation(CSV.readMat(rotFile),CSV.readMat(normFile))(morpho)



  /**
    * Number of connected components
    * @param world world
    * @param cachedNetwork network if cached
    * @return
    */
  def components(world: Array[Array[Double]],cachedNetwork: Option[Network] = None): Double = {
    val nw = cachedNetwork match {case None => network.gridToNetwork(world);case n => n.get}
    val components = GraphAlgorithms.connectedComponents(nw)
    components.size
  }

  /**
    * average block area
    * @param world world
    * @return
    */
  def avgBlockArea(world: Array[Array[Double]],cachedNetwork: Option[Network] = None): Double = {
    val nw = cachedNetwork match {case None => network.gridToNetwork(world);case n => n.get}
    val components = GraphAlgorithms.connectedComponents(nw)
    val avgblockarea = components.size match {case n if n == 0 => 0.0;case _ => components.map{_.nodes.size}.sum/components.size.toDouble}
    avgblockarea
  }

  /**
    * avg component area
    * @param world world
    * @return
    */
  def avgComponentArea(world: Array[Array[Double]]): Double = {
    val inversedNetwork = network.gridToNetwork(world.map{_.map{x => 1.0 - x}})
    val components = GraphAlgorithms.connectedComponents(inversedNetwork)
    if(components.nonEmpty){
      components.map{_.nodes.size}.sum/components.size
    }else 0.0
  }


  /**
    * Average detour compared to euclidian
    *  - too costly to do all shortest paths => sample
    *  - should sample points within connected components?
    * @param world world
    * @param cachedNetwork cached network
    * @param sampledPoints number of sampling points
    * @return
    */
  def avgDetour(world: Array[Array[Double]],cachedNetwork: Option[Network] = None,sampledPoints: Int=50)(implicit rng: Random): Double = {
    if(world.flatten.sum==world.map{_.length}.sum) return 0.0
    val nw = cachedNetwork match {case None => network.gridToNetwork(world);case n => n.get}
    val sampled = nw.nodes.sampleWithoutReplacement(sampledPoints)(rng)
    val paths = GraphAlgorithms.shortestPaths(nw, sampled, sampled)
    val avgdetour = paths.toSeq.filter{!_._2._3.isInfinite}.map{
      case (_,(nodes,_,d))=>
        val (n1,n2) = (nodes(0),nodes.last)
        val de = math.sqrt((n1.x-n2.x)*(n1.x-n2.x)+(n1.y-n2.y)*(n1.y-n2.y))
        d/de
    }.filter{!_.isNaN}.filter{!_.isInfinite}.sum / paths.size
    avgdetour
  }



  /**
    * Global density
    * @param world world
    * @return
    */
  def density(world: Array[Array[Double]]): Double = world.flatten.filter(_ > 0.0).map{_ => 1.0}.sum / world.flatten.length




  /**
    * Mean distance using fast convolution.
    *
    * @param matrix matrix
    * @return
    */
  def distanceMean(matrix: Array[Array[Double]],normalize: Boolean = true): Double = {
    val x = matrix.map{_.map{d => if(d.isNaN) 0.0 else d}}
    val totPop = x.flatten.sum
    val dmat = distanceMatrix(2 * x.length - 1,2 * x(0).length - 1)
    val conv = Convolution.convolution2D(x, dmat)
    val norm = if (normalize) math.sqrt(math.Pi / x.flatten.length) else 1.0
    norm / (totPop * totPop) * MathArrays.ebeMultiply(conv.flatten, x.flatten).sum
  }


  /**
    * Acentrism index
    *   Le Néchet, F. (2015). De la forme urbaine à la structure métropolitaine: une typologie de la configuration interne des densités pour les principales métropoles européennes de l’Audit Urbain. Cybergeo: European Journal of Geography.
    *    - simplified as requires computation of average distance for each quantile considered
    */
  def acentrism(matrix: Array[Array[Double]], quantiles: Array[Double] = Array.tabulate(100){n => n*0.01}): Double = {
    val popdists = quantiles.map{ q =>
      val posvalues = matrix.flatten.filter(_ > 0).sorted(Ordering.Double.TotalOrdering)
      val qth = posvalues((q*posvalues.length).toInt)
      val filteredmat = matrix.map(_.map{d => if (d < qth) 0.0 else d})
      (posvalues.sum,distanceMean(filteredmat,normalize = false))
    }
    val (totpop,totdist)=popdists(1)
    popdists.tail.zip(popdists.dropRight(1)).map{case ((pi1,di1),(pi,di)) => (pi - pi1)*(di + di1) / (2*totpop*totdist)}.sum
  }


  /**
    * Box counting fractal dimension using convolution
    *
    * ! finish implementation
    *
    * @return
    */
  def fractalDimension(matrix: Array[Array[Double]]): (Double,Double) = {
    val maxkernelsize = math.floor(math.min(matrix.length,matrix(0).length) / 4) - 1
    val _ = (1 to maxkernelsize.toInt by 1).map{k: Int =>
      val convol: Array[Array[Double]] = Convolution.convolution2D(matrix,Array.fill(2*k+1){Array.fill(2*k+1)(1.0)})
      val counts = convol.map(_.zipWithIndex).zipWithIndex.flatMap{
        //  this is terrible - find why scala2.13 consider full pattern matching not with return type but Any
        rowind: (Array[(Double,Int)],Int) => rowind._1.map{case (d,j) => var res = 0.0; if (rowind._2%(2*k+1)==k&&j%(2*k+1)==k) {if(d > 0.0) res = 1.0}; res}
      }.sum
      (2*k+1,counts)
    }
    (0.0,0.0)
  }

  /**
    * aggregated gravity flow with simple square externalities (linear utility)
    *
    *  Future work
    *  - generalize to any utility function ? may be trickier to compute for other things than polynomials ?
    *
    * @param matrix matrix
    * @param congestionCost cost of congestion parameter
    * @return
    */
  def congestedFlows(matrix: Array[Array[Double]],congestionCost: Double): Double = {
    val totPop = matrix.flatten.sum
    if(totPop==0.0){0.0} else {
      val dmat = distanceMatrix(2 * matrix.length - 1,2 * matrix(0).length - 1)
      val conv = Convolution.convolution2D(matrix, dmat.map {_.map {d => if (d==0) 0.0 else 1 / d}})
      val flows = MathArrays.ebeMultiply(conv.flatten, matrix.flatten).sum / (totPop * totPop)
      val convsquared = Convolution.convolution2D(matrix.map {
        _.map {
          math.pow(_, 2)
        }
      }, dmat.map {
        _.map { d => if(d==0) 0.0 else math.pow(1/d, 2.0) }
      })
      val cong = MathArrays.ebeMultiply(convsquared.flatten, matrix.map {
        _.map {
          math.pow(_, 2)
        }
      }.flatten).sum / math.pow(totPop, 4)
      flows - congestionCost * cong
    }
  }

  /**
    * Distance kernel
    *
    * @param n rows
    * @param p cols
    * @return
    */
  def distanceMatrix(n: Int, p: Int): Array[Array[Double]] = {
    Array.tabulate(n, p) { (i, j) => math.sqrt((i - n / 2) * (i - n / 2) + (j - p / 2) * (j - p / 2)) }
  }



  /**
    * Moran index using fast convolution.
    *
    * @param matrix matrix
    * @return
    */
  def moran(matrix: Array[Array[Double]],weightFunction: Array[Array[Double]]=> Array[Array[Double]] = spatialWeights): Double = {
    val x = matrix.map{_.map{d => if(d.isNaN) 0.0 else d}}
    val flatConf = x.flatten
    val popMean = flatConf.sum / flatConf.length
    val centeredConf = x.map { r => r.map { d => d - popMean }}
    val variance = MathArrays.ebeMultiply(centeredConf.flatten, centeredConf.flatten).sum
    val weights = weightFunction(x)
    val totWeight = Convolution.convolution2D(Array.fill(x.length, x(0).length) { 1.0 }, weights).flatten.sum
    flatConf.length / (totWeight * variance) * MathArrays.ebeMultiply(centeredConf.flatten, Convolution.convolution2D(centeredConf, weights).flatten).sum
  }

  /**
    * Default spatial weights for Moran 1/d_{ij}
    *
    * @param matrix matrix
    * @return
    */
  def spatialWeights(matrix: Array[Array[Double]]): Array[Array[Double]] = {
    val (n,p) = (2 * matrix.length - 1,2 * matrix(0).length - 1)
    Array.tabulate(n, p) { (i, j) => if (i == n / 2 && j == p / 2) 0.0 else 1 / math.sqrt((i - n / 2) * (i - n / 2) + (j - p / 2) * (j - p / 2))}
  }




  /**
    * Average distance between individuals in the population
    * (direct computation)
    *
    * @param matrix matrix
    * @return
    */
  def distanceMeanDirect(matrix: Array[Array[Double]]): Double = {

    def totalQuantity = matrix.flatten.sum

    def numerator =
      (for {
        (c1, p1) <- zipWithPosition(matrix)
        (c2, p2) <- zipWithPosition(matrix)
      } yield distance(p1, p2) * c1 * c2).sum

    def normalisation = math.sqrt(matrix.flatten.length / math.Pi)

    if(totalQuantity==0.0||normalisation==0.0) return 0.0

    (numerator / (totalQuantity * totalQuantity)) / normalisation
  }

  def distance(p1: (Int,Int), p2: (Int,Int)): Double = {
    val (i1, j1) = p1
    val (i2, j2) = p2
    val a = i2 - i1
    val b = j2 - j1
    math.sqrt(a * a + b * b)
  }

  def zipWithPosition(m :Array[Array[Double]]): Seq[(Double, (Int,Int))] = {
    m.zipWithIndex.map{
      case (row,i) =>
        row.zipWithIndex.map{
          case (content,j) => (content,(i,j))
        }
    }.flatten.toSeq
  }


  /**
    * Direct computation of Moran index (in O(N4))
    * @param matrix matrix
    * @return
    */
  def moranDirect(matrix: Array[Array[Double]]): Double = {
    def decay(p1:(Int,Int),p2:(Int,Int)): Double = {
      if (p1==p2) 0.0
      else 1/distance(p1,p2)
    }
    def flatCells = matrix.flatten
    val totalPop = flatCells.sum
    val averagePop = totalPop / matrix.flatten.length


    def vals =
      for {
        (c1, p1) <- zipWithPosition(matrix)
        (c2, p2) <- zipWithPosition(matrix)
      } yield (decay(p1, p2) * (c1 - averagePop) * (c2 - averagePop),decay(p1, p2))



    def numerator : Double = vals.map{case (n,_)=>n}.sum
    def totalWeight : Double = vals.map{case(_,w)=>w}.sum

    def denominator =
      flatCells.map {
        p =>
          if (p == 0) 0
          else math.pow(p - averagePop.toDouble, 2)
      }.sum

    if (denominator == 0) 0.0
    else (matrix.flatten.length / totalWeight) * (numerator / denominator)
  }


  val CROSS_KERNEL: Array[Array[Double]] = Array(Array(0.0,1.0,0.0),Array(1.0,1.0,1.0),Array(0.0,1.0,0.0))

  /**
    * Dilation with default cross mask
    * @param matrix matrix
    * @param convol convolution function
    * @return
    */
  def dilation(matrix: Array[Array[Double]],
               convol: (Array[Array[Double]],Array[Array[Double]],Double => Double)=> Array[Array[Double]] = Convolution.convolution2dDirect
              ): Array[Array[Double]] =
    convol(matrix,CROSS_KERNEL,{d => if(d > 0.0) 1.0 else 0.0})

  def erosion(matrix: Array[Array[Double]],
              convol: (Array[Array[Double]],Array[Array[Double]],Double=> Double)=> Array[Array[Double]] = Convolution.convolution2dDirect
             ): Array[Array[Double]] =
    convol(matrix,
      CROSS_KERNEL,
      {d => if (d == CROSS_KERNEL.flatten.sum) 1.0 else 0.0 }
    )


  /**
    * Number of steps to fully close the image (morpho maths)
    *
    * @param matrix matrix
    * @param convol convolution function
    * @return
    */
  def fullDilationSteps(matrix: Array[Array[Double]],
                        convol: (Array[Array[Double]],Array[Array[Double]],Double=> Double)=> Array[Array[Double]] = Convolution.convolution2dDirect
                       ): Double = {
    var steps = 0
    var complete = false
    var currentworld = matrix
    if(matrix.flatten.sum==0){return 0.0}
    while(!complete){
      currentworld = dilation(currentworld,convol)
      complete = currentworld.flatten.sum == currentworld.flatten.length
      steps = steps + 1
    }
    steps
  }

  /**
    * Number of steps to fully erode the image
    * @param matrix matrix
    * @return
    */
  def fullErosionSteps(matrix: Array[Array[Double]],
                       convol: (Array[Array[Double]],Array[Array[Double]],Double=> Double)=> Array[Array[Double]] = Convolution.convolution2dDirect
                      ): Double = {
    var steps = 0
    var complete = false
    var currentworld = matrix
    if(matrix.flatten.sum==matrix.flatten.length){return 0.0}
    while(!complete){
      currentworld = erosion(currentworld,convol)
      complete = currentworld.flatten.sum == 0
      steps = steps + 1
    }
    steps
  }


  /**
    * Closing is the erosion of the dilation
    *
    * @param matrix matrix
    * @return
    */
  def fullClosingSteps(matrix: Array[Array[Double]],
                       convol: (Array[Array[Double]],Array[Array[Double]],Double=> Double)=> Array[Array[Double]] = Convolution.convolution2dDirect
                      ): Double = {
    var steps = 0
    var complete = false
    var currentworld = matrix
    if(matrix.flatten.sum==0.0){return 0.0} // Double.PositiveInfinity - by convention return 0 instead of infty for easier reading of csv files
    while(!complete){
      val prevworld = currentworld.map{_.clone()}
      currentworld = erosion(dilation(currentworld,convol),convol)
      val diff = prevworld.zip(currentworld).map{case (d1,d2) => d1.zip(d2).map{case (dd1,dd2)=> math.abs(dd1-dd2)}.sum}.sum
      complete = diff==0.0
      steps = steps + 1
    }
    steps
  }


  /**
    * Opening is dilating the erosion
    * @param matrix matrix to open
    * @param convol convolution function
    * @return
    */
  def fullOpeningSteps(matrix: Array[Array[Double]],
                       convol: (Array[Array[Double]],Array[Array[Double]],Double=> Double)=> Array[Array[Double]] = Convolution.convolution2dDirect
                      ): Double = {
    var steps = 0
    var complete = false
    var currentworld = matrix
    if(matrix.flatten.sum==matrix.flatten.length){return 0.0}
    while(!complete){
      val prevworld = currentworld.map{_.clone()}
      currentworld = dilation(erosion(currentworld,convol),convol)
      val diff = prevworld.zip(currentworld).map{case (d1,d2) => d1.zip(d2).map{case (dd1,dd2)=> math.abs(dd1-dd2)}.sum}.sum
      complete = diff==0.0
      steps = steps + 1
    }
    steps
  }








}
