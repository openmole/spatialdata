
package spatialdata.measures

import org.apache.commons.math3.linear.MatrixUtils
import spatialdata.utils.math.Convolution
import org.apache.commons.math3.stat.regression.SimpleRegression
import org.apache.commons.math3.util.MathArrays
import spatialdata.{RasterLayerData, measures}
import spatialdata.network.Network
import spatialdata.utils.io.CSV

import scala.math._


case class Morphology(
                       height: Double,
                       width: Double,
                       area: Double,
                       moran: Double,
                       avgDistance: Double,
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

}


/**
  * Morphological indicators for spatial density grids
  *
  * (see Raimbault, J. (2018). Calibration of a density-based model of urban morphogenesis. PloS one, 13(9), e0203516.)
  *
  * FIXME See https://github.com/locationtech/geotrellis if implemented in raster operations
  *
  */
object Morphology {

  def apply(grid: RasterLayerData[Double]): Morphology = {
    val cachedNetwork = Network.gridToNetwork(grid)
    Morphology(
      grid.size,grid(0).size,
      grid.flatten.sum,
      moranDirect(grid),
      distanceMeanDirect(grid),
      density(grid),
      components(grid,Some(cachedNetwork)),
      avgDetour(grid,Some(cachedNetwork)),
      avgBlockArea(grid,Some(cachedNetwork)),
      avgComponentArea(grid),
      fullDilationSteps(grid),
      fullErosionSteps(grid),
      fullClosingSteps(grid),
      fullOpeningSteps(grid)
    )
  }

  /**
    * read a rotation from file and perform it on the normalized corresponding components
    * (can be projected if not same number of arrows as number of elements in Morphology)
    * @param rotFile
    * @param morpho
    * @return
    */
  def rotation(rotFile: String,normFile: String)(morpho: Morphology): Array[Double] = {
    val rotation = MatrixUtils.createRealMatrix(CSV.readMat(rotFile))
    val n = rotation.getRowDimension
    val norms = CSV.readMat(normFile)
    val morphonorm = morpho.toArray(n).zip(norms).map{case (m,a) => (a(0)-m)/(a(0)-a(1))}
    println("normalized : "+morphonorm.toSeq)
    val rotated = MatrixUtils.createRowRealMatrix(morphonorm).multiply(rotation).getRow(0)
    println("rotated : "+rotated.toSeq)
    rotated
  }




  /**
    * Number of connected components
    * @param world
    * @param cachedNetwork
    * @return
    */
  def components(world: Array[Array[Double]],cachedNetwork: Option[Network] = None): Double = {
    val network = cachedNetwork match {case None => Network.gridToNetwork(world);case n => n.get}
    val components = Network.connectedComponents(network)
    //println("components = "+components.size)
    components.size
  }

  /**
    * average block area
    * @param world
    * @return
    */
  def avgBlockArea(world: Array[Array[Double]],cachedNetwork: Option[Network] = None): Double = {
    //val inversedNetwork = Network.gridToNetwork(world.map{_.map{case x => 1.0 - x}})
    val network = cachedNetwork match {case None => Network.gridToNetwork(world);case n => n.get}
    val components = Network.connectedComponents(network)
    val avgblockarea = components.size match {case n if n == 0 => 0.0;case n => components.map{_.nodes.size}.sum/components.size}
    //println("avgblockarea = "+avgblockarea)
    avgblockarea
  }

  /**
    * avg component area
    * @param world
    * @return
    */
  def avgComponentArea(world: Array[Array[Double]]): Double = {
    val inversedNetwork = Network.gridToNetwork(world.map{_.map{case x => 1.0 - x}})
    val components = Network.connectedComponents(inversedNetwork)
    //println("avgblockarea = "+avgblockarea)
    if(components.size > 0){
      components.map{_.nodes.size}.sum/components.size
    }else 0.0
  }


  /**
    * average detour compared to euclidian
    * @param world
    * @param cachedNetwork
    * @param sampledPoints
    * @return
    */
  def avgDetour(world: Array[Array[Double]],cachedNetwork: Option[Network] = None,sampledPoints: Int=50): Double = {
    if(world.flatten.sum==world.map{_.length}.sum){return(0.0)}
    val network = cachedNetwork match {case None => Network.gridToNetwork(world);case n => n.get}
    // too costly to do all shortest paths => sample
    //val shortestPaths = Network.allPairsShortestPath(network)
    //val avgdetour = shortestPaths.values.map{_.map{_.weight}.sum}.zip(shortestPaths.keys.map{case (n1,n2)=> math.sqrt((n1.x-n2.x)*(n1.x-n2.x)+(n1.y-n2.y)*(n1.y-n2.y))}).map{case (dn,de)=>dn/de}.sum/shortestPaths.size
    //println("avgdetour = "+avgdetour)
    // should sample points within connected components
    val sampled = network.nodes.toSeq.take(sampledPoints)
    val paths = Network.shortestPathsScalagraph(network,sampled)

    val avgdetour = paths.filter{!_._2._2.isInfinite}.map{
      case (_,(nodes,d))=>
        val (n1,n2) = (nodes(0),nodes.last)
        val de = math.sqrt((n1.x-n2.x)*(n1.x-n2.x)+(n1.y-n2.y)*(n1.y-n2.y))
        //println(d,de)
        d/de
    }.filter{!_.isNaN}.filter{!_.isInfinite}.sum / paths.size
    avgdetour
  }



  /**
    * Global density
    * @param world
    * @return
    */
  def density(world: Array[Array[Double]]): Double = world.flatten.map{x => if(x>0.0)1.0 else 0.0}.sum / world.flatten.size




  /**
    * Mean distance using fast convolution.
    *
    * @param matrix
    * @return
    */
  def distanceMean(matrix: Array[Array[Double]]): Double = {
    val totPop = matrix.flatten.sum
    val dmat = distanceMatrix(2 * matrix.length - 1)
    val conv = Convolution.convolution2D(matrix, dmat)
    math.sqrt(math.Pi) / (matrix.length * totPop * totPop) * MathArrays.ebeMultiply(conv.flatten, matrix.flatten).sum
  }

  /**
    * Distance kernel
    *
    * @param n
    * @return
    */
  def distanceMatrix(n: Int): Array[Array[Double]] = {
    Array.tabulate(n, n) { (i, j) => math.sqrt((i - n / 2) * (i - n / 2) + (j - n / 2) * (j - n / 2)) }
  }



  /**
    * Moran index using fast convolution.
    *
    * @param matrix
    * @return
    */
  def moran(matrix: Array[Array[Double]],weightFunction: Array[Array[Double]]=> Array[Array[Double]] = spatialWeights): Double = {
    val n = matrix.length
    val flatConf = matrix.flatten
    val popMean = flatConf.sum / flatConf.length
    val centeredConf = matrix.map { r => r.map { d => d - popMean } }
    val variance = MathArrays.ebeMultiply(centeredConf.flatten, centeredConf.flatten).sum
    val weights = weightFunction(matrix)
    val totWeight = Convolution.convolution2D(Array.fill(n, n) { 1.0 }, weights).flatten.sum
    flatConf.length / (totWeight * variance) * MathArrays.ebeMultiply(centeredConf.flatten, Convolution.convolution2D(centeredConf, weights).flatten).sum
  }

  /**
    * Default spatial weights for Moran
    * @param n
    * @return
    */
  def spatialWeights(matrix: Array[Array[Double]]): Array[Array[Double]] = {
    val n:Int = 2 * matrix.length - 1
    Array.tabulate(n, n) { (i, j) => if (i == n / 2 && j == n / 2) 0.0 else 1 / math.sqrt((i - n / 2) * (i - n / 2) + (j - n / 2) * (j - n / 2)) }
  }




  /**
    * Average distance between individuals in the population
    * (direct computation)
    *
    * @param matrix
    * @return
    */
  def distanceMeanDirect(matrix: Array[Array[Double]]): Double = {

    def totalQuantity = matrix.flatten.sum

    def numerator =
      (for {
        (c1, p1) <- zipWithPosition(matrix)
        (c2, p2) <- zipWithPosition(matrix)
      } yield distance(p1, p2) * c1 * c2).sum

    def normalisation = matrix.length / math.sqrt(math.Pi)

    if(totalQuantity==0.0||normalisation==0.0) return(0.0)

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
    for {
      (row, i) <- m.zipWithIndex
      (content, j) <- row.zipWithIndex
    } yield (content,(i, j))
  }


  /**
    * Direct computation of Moran index (in O(N^4))
    * @param matrix
    * @return
    */
  def moranDirect(matrix: Array[Array[Double]]): Double = {
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

    if (denominator == 0) 0
    else (matrix.flatten.length / totalWeight) * (numerator / denominator)
  }

  def decay(p1:(Int,Int),p2:(Int,Int)) = {
    if (p1==p2) 0.0
    else 1/distance(p1,p2)
  }



  /**
    * Naive two dimensional convolution for morpho math - default operator is average (dilation) - replace by product for erosion
    *   (not efficient at all but no math commons to work in the gui)
    * @param matrix
    * @param mask should be of uneven size
    * @return
    */
  def convolution(matrix: Array[Array[Double]],mask: Array[Array[Double]],operator: Array[Double]=>Double = {case a => if(a.filter(_>0.0).size>0)1.0 else 0.0}): Array[Array[Double]] = {
    assert(mask.length%2==1&&mask(0).length%2==1,"mask should be of uneven size")
    val sizes = matrix.map(_.length);assert(sizes.max==sizes.min,"array should be rectangular")
    val masksizes = mask.map(_.length);assert(masksizes.max==masksizes.min,"mask should be rectangular")
    val (paddingx,paddingy) = ((mask.length-1)/2,(mask(0).length-1)/2)
    val padded = Array.tabulate(matrix.length+2*paddingx,matrix(0).length+2*paddingy){
      case (i,j) if i<paddingx||i>=(matrix.length+paddingx)||j<paddingy||j>=(matrix(0).length+paddingy) => 0.0
      case (i,j) => matrix(i-paddingx)(j-paddingy)
    }
    val res = Array.fill(matrix.length+2*paddingx,matrix(0).length+2*paddingy)(0.0)
    for(i <- paddingx until (res.length - paddingx);j <- paddingy until (res(0).length-paddingy)){
      val masked = Array.fill(mask.size,mask(0).size)(0.0)
      for(k <- - paddingx to paddingx;l <- - paddingy to paddingy){
        //assert(i+k<matrix.length&j+l<matrix(0).length,"size : "+i+" "+j+" "+k+" "+" "+l+" for a matrix of size "+matrix.length+";"+matrix(0).length)
        masked(k+paddingx)(l+paddingy)=padded(i+k)(j+l)*mask(k+paddingx)(l+paddingy)
      }
      res(i)(j) = operator(masked.flatten)
    }
    //res.zip(matrix).map{case (row,initrow) => row.take(initrow.length + paddingy).takeRight(initrow.length)}.take(matrix.length+paddingx).takeRight(matrix.length)
    res.map{case row => row.slice(paddingy,row.length-paddingy)}.slice(paddingx,res.length-paddingx)
  }

  /**
    * Dilation with default cross mask
    * @param matrix
    * @return
    */
  def dilation(matrix: Array[Array[Double]]): Array[Array[Double]] = convolution(matrix,Array(Array(0.0,1.0,0.0),Array(1.0,1.0,1.0),Array(0.0,1.0,0.0)))

  def erosion(matrix: Array[Array[Double]]): Array[Array[Double]] = {
    val mask = Array(Array(0.0, 1.0, 0.0), Array(1.0, 1.0, 1.0), Array(0.0, 1.0, 0.0))
    convolution(matrix,
      mask,
      { case a => if (a.filter(_ > 0.0).sum == mask.flatten.sum) 1.0 else 0.0 }
    )
  }

  /**
    * Number of steps to fully close the image (morpho maths)
    *
    * @param matrix
    * @return
    */
  def fullDilationSteps(matrix: Array[Array[Double]]): Double = {
    var steps = 0
    var complete = false
    var currentworld = matrix
    //if(matrix.flatten.sum==0){return(Double.PositiveInfinity)}
    if(matrix.flatten.sum==0){return(0.0)}
    while(!complete){
      //println("dilating "+steps+" ; "+currentworld.flatten.sum+"/"+currentworld.flatten.length+" ; "+currentworld.length+" - "+currentworld(0).length)
      //println(Grid.gridToString(currentworld)+"\n\n")
      currentworld = dilation(currentworld)
      complete = currentworld.flatten.sum == currentworld.flatten.length
      steps = steps + 1
    }
    steps
  }

  /**
    * Number of steps to fully erode the image
    * @param matrix
    * @return
    */
  def fullErosionSteps(matrix: Array[Array[Double]]): Double = {
    var steps = 0
    var complete = false
    var currentworld = matrix
    //if(matrix.flatten.sum==matrix.flatten.length){return(Double.PositiveInfinity)}
    if(matrix.flatten.sum==matrix.flatten.length){return(0.0)}
    while(!complete){
      //println("eroding "+steps+" ; "+currentworld.flatten.sum+"/"+currentworld.flatten.length)
      //println(Grid.gridToString(currentworld)+"\n\n")
      currentworld = erosion(currentworld)
      complete = currentworld.flatten.sum == 0
      steps = steps + 1
    }
    steps
  }


  /**
    * Closing is the erosion of the dilation
    *
    * @param matrix
    * @return
    */
  def fullClosingSteps(matrix: Array[Array[Double]]): Double = {
    var steps = 0
    var complete = false
    var currentworld = matrix
    //if(matrix.flatten.sum==0.0){return(Double.PositiveInfinity)}
    if(matrix.flatten.sum==0.0){return(0.0)} // by convention return 0 instead of infty for easier reading of csv files
    while(!complete){
      //println("closing "+steps+" ; "+currentworld.flatten.sum+"/"+currentworld.flatten.length)
      //println(Grid.gridToString(currentworld)+"\n\n")
      val prevworld = currentworld.map{_.clone()}
      currentworld = erosion(dilation(currentworld))
      val diff = prevworld.zip(currentworld).map{case (d1,d2) => d1.zip(d2).map{case (dd1,dd2)=> math.abs(dd1-dd2)}.sum}.sum
      //println("diff = "+diff)
      complete = (diff==0.0)
      steps = steps + 1
    }
    steps
  }


  /**
    * Opening is dilating the erosion
    * @param matrix
    * @return
    */
  def fullOpeningSteps(matrix: Array[Array[Double]]): Double = {
    var steps = 0
    var complete = false
    var currentworld = matrix
    //if(matrix.flatten.sum==matrix.flatten.length){return(Double.PositiveInfinity)}
    if(matrix.flatten.sum==matrix.flatten.length){return(0.0)}
    while(!complete){
      //println("opening "+steps+" ; "+currentworld.flatten.sum+"/"+currentworld.flatten.length)
      //println(Grid.gridToString(currentworld)+"\n\n")
      val prevworld = currentworld.map{_.clone()}
      currentworld = dilation(erosion(currentworld))
      val diff = prevworld.zip(currentworld).map{case (d1,d2) => d1.zip(d2).map{case (dd1,dd2)=> math.abs(dd1-dd2)}.sum}.sum
      //println("diff = "+diff)
      complete = (diff==0.0)
      steps = steps + 1
    }
    steps
  }








}
