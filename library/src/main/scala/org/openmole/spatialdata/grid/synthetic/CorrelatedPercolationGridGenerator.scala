package org.openmole.spatialdata.grid.synthetic

import org.apache.commons.math3.complex.Complex
import org.apache.commons.math3.special.Gamma
import net.sourceforge.jdistlib.math.Bessel
import org.apache.commons.math3.transform.{DftNormalization, FastFourierTransformer, TransformType, TransformUtils}
import org.openmole.spatialdata.grid.measures.GridMorphology
import org.openmole.spatialdata.grid.{GridGenerator, RasterLayerData}

import scala.util.Random
import scala.annotation.nowarn


/**
  *
  * Implementation of the correlated percolation urban model described in
  *   Makse, H. A., Andrade, J. S., Batty, M., Havlin, S., & Stanley, H. E. (1998).
  *   Modeling urban growth patterns with correlated percolation. Physical Review E, 58(6), 7054.
  *  ( see Makse, H. A., Havlin, S., Schwartz, M., & Stanley, H. E. (1996). Method for generating long-range correlations for large systems. Physical Review E, 53(5), 5445. for more details on correlated field generation)
  *
  * Model is generalized by
  *  - adding an option to produce density values instead of a binary variable, by multiplying by the density gradient
  *  - polycentric density gradient, for which radiuses scale such that theoretical population in each center scale, i.e. such that
  *     lambda_i = \sqrt{2pi P_0/P_m i -alpha} = lambda_0 i -alpha/2
  *
  * Specific morphology indicators from the paper? study scaling exponents, require experiments varying lambda and alpha
  *
  *
  * @param gridSize grid size
  * @param maxKernelRadius gradient of the exponential density mask
  * @param correlationRange correlation range
  * @param maxPopulation used when not binary, to rescale occupied cells with the population gradient P_max exp(-lambda r)
  * @param binary binary or rescaled population cells
  */
case class CorrelatedPercolationGridGenerator(
                                             gridSize: Int,
                                             correlationRange: Double,
                                             maxPopulation: Double = 1.0,
                                             binary: Boolean = true,
                                             nCenters: Int = 1,
                                             centersPopulationScaling: Double = 1.0,
                                             maxKernelRadius: Double = 1.0
                                             ) extends GridGenerator {

  override def generateGrid(implicit rng: Random): RasterLayerData[Double] =
    CorrelatedPercolationGridGenerator.correlatedPercolationGrid(gridSize, maxKernelRadius, correlationRange, maxPopulation, binary, nCenters, centersPopulationScaling)

}



object CorrelatedPercolationGridGenerator {

  /**
    * Polycentric correlated percolation combining: polycentric probability density with a scaling law with a correlated random field
    * @param gridSize size
    * @param maxKernelRadius density gradient (maximal value among centers)
    * @param correlationRange correlation range
    * @param maxPopulation max population
    * @param binary binary output?
    * @param nCenters number of centers
    * @param centersPopulationScaling scaling of centers
    * @param rng rng
    * @return
    */
  def correlatedPercolationGrid(gridSize: Int,
                                maxKernelRadius: Double,
                                correlationRange: Double,
                                maxPopulation: Double,
                                binary: Boolean,
                                nCenters: Int,
                                centersPopulationScaling: Double
                               )(implicit rng: Random): Array[Array[Double]] = {
    //val pr = GridMorphology.distanceMatrix(gridSize, gridSize).map(_.map(r => math.exp(-densityGradient*r))) // for one center at the origin
    val kernelSizes = (1 to nCenters).map(i => maxKernelRadius/math.pow(i, centersPopulationScaling / 2))
    val pr: Array[Array[Double]] = ExpMixtureGridGenerator(Left(gridSize), nCenters, 1.0, kernelSizes).generateGrid
    // need to renormalize in case of more than one center
    val m = pr.flatten.max(Ordering.Double.TotalOrdering)
    val prnorm = pr.map(_.map(_/m))

    val bin = density(prnorm, correlatedField(gridSize, correlationRange))

    if (binary) bin
    else {
      bin.zip(prnorm).map{(rows: (Array[Double],Array[Double])) =>
        rows._1.zip(rows._2).map(d=> d._1*d._2*maxPopulation)
      }
    }
  }


  /**
    * use density probabilities and correlated field to generate binary settlement result
    * @param densityProbas p(r) in the paper
    * @param field correlated field eta(r)
    * @return
    */
  def density(
               densityProbas: Array[Array[Double]],
               field: Array[Array[Double]]
              ): Array[Array[Double]] = {


    val n = field.flatten.length.toDouble
    val sortedeta = field.flatten.groupBy(eta => eta).toIndexedSeq.sortBy(_._1)(Ordering.Double.TotalOrdering)
    def cumcount(s: (IndexedSeq[Array[Double]],Double)): (IndexedSeq[Array[Double]],Double) = {
      if (s._1.nonEmpty) (s._1.tail, s._2+s._1.head.length.toDouble / n) else s
    }
    val cdf = Iterator.iterate((sortedeta.map(_._2), 0.0))(cumcount).takeWhile(_._1.nonEmpty).toSeq.map(_._2).zip(sortedeta.map(_._1))
    //println(cdf.length+" / "+sortedeta.size)
    //println(cdf)
    def theta(p: Double): Double = {
      val i = cdf.indexWhere(_._1>=p)
      if(i>=0)cdf(i)._2 else cdf.last._2
    }
    val thetas = densityProbas.map(_.map(theta))
    def hside(th: Double, eta: Double): Double = if (th >= eta) 1.0 else 0.0
    thetas.zip(field).map{(rows: (Array[Double],Array[Double])) =>
      rows._1.zip(rows._2).map(d=> hside(d._1,d._2))
    }
  }

  /**
    *  Basic masking - does not take into account actual distribution of eta
    * @param gridSize grid size
    * @param densityGradient density gradient
    * @param field field
    * @return
    */
  def densityMask(gridSize: Int,
                  densityGradient: Double,
                  field: Array[Array[Double]]
                 ): Array[Array[Double]] = {
    GridMorphology.distanceMatrix(gridSize, gridSize).map(_.map(r => math.exp(-densityGradient*r))).zip(field).map{
      (rows: (Array[Double],Array[Double])) => rows._1.zip(rows._2).map{
        (d: (Double,Double)) =>
          d._2 match {
            case ff if ff > d._1 => 1.0
            case _ => 0.0
          }
      }
    }
  }

  /**
    * Generation of spatially correlated random variable using Fourier transform
    *
    *  - generate random field (Gaussian distribution) u(r)
    *  - compute Fourier transform u(q)
    *  - compute Spectral density S(q) = 2sqrt(pi)/Gamma(beta+1) (q/2)beta K_beta (q) with K_beta modified Bessel function
    *  - compute eta(q) = sqrt(S(q)) u(q)
    *  - invert to obtain correlated field eta(r) = Fourier-1[eta(q)]
    *
    * @param gridSize grid size
    * @param correlationRange correlation range
    * @param rng rng
    * @return
    */
  @nowarn def correlatedField(gridSize: Int, correlationRange: Double)(implicit rng: Random): Array[Array[Double]] = {
    // generate random field
    val randomField = Array.fill(gridSize,gridSize)(rng.nextGaussian())

    // Fourier transform
    val tr = new FastFourierTransformer(DftNormalization.STANDARD)
    val cflength = math.pow(2.0, math.ceil(math.log(gridSize) / math.log(2.0))).toInt
    //println(cflength)
    val cf = randomField.map(r => TransformUtils.createComplexArray(Array(r.padTo(cflength, 0.0), Array.fill(cflength)(0.0)))).
      padTo(cflength,TransformUtils.createComplexArray(Array(Array.fill(cflength)(0.0),Array.fill(cflength)(0.0))))
    // mdfft deprecated: no replacement?
    val transformedField: Array[Array[Complex]] = tr.mdfft(cf, TransformType.FORWARD).asInstanceOf[Array[Array[Complex]]]

    // compute spectral density
    // modified Bessel function given by K_alpha (x) = pi/2 i^{alpha + 1} H_alpha (x)
    // with H_alpha (x) = J_alpha(x) + i Y_alpha(x) and Y_alpha(x) = J_alpha(x) cos(pi alpha) - J_{- alpha} (x) / sin(pi alpha)
    val beta = (correlationRange - 2.0) / 2.0
    val gamma = Gamma.gamma(beta + 1.0)
    //println("gamma = "+gamma)
    val qij = Array.tabulate(cflength,cflength){case (i,j) => math.sqrt(math.pow(2*math.Pi*(i - (cflength.toDouble / 2.0))/cflength.toDouble, 2.0) + math.pow(2*math.Pi*(j - (cflength.toDouble / 2.0))/cflength.toDouble, 2.0))}
    val qijvals = qij.flatten.groupBy(d => d).keySet.toSeq
    //println(qijvals)
    val qijmap = qijvals.zipWithIndex.toMap
    //qij.map(_.zipWithIndex).zipWithIndex.map{case (r,i) => r.map{case (d,j) => ((i, j), d)}}.flatten
    // math commons does not take negative order
    //val Jalpha = qijvals.map{q => BesselJ.value(beta, q)}
    //val Jminalpha = qijvals.map{q => BesselJ.value( - beta, q)}
    //val Yalpha = Jalpha.zip(Jminalpha).map{case (ja, jma) => (ja*math.cos(math.Pi * beta) - jma) / math.sin(math.Pi * beta)}
    //val Halpha = Jalpha.zip(Yalpha).map{case (ja,ya) => new Complex(ja, ya)}
    //val Kalpha = Halpha.map(_.multiply(Complex.I.pow(new Complex(beta + 1, 0.0)).multiply(math.Pi/2.0)))
    // use jdistlib instead
    val Kalpha = qijvals.map(q => Bessel.k(q, beta, false))
    //println(qijvals(Kalpha.indexWhere(_.isInfinite)))
    val sqrtSqijvals = qijvals.zip(Kalpha).map{
      case (q, ka) => ka match {
        case x if x.isInfinite => 2 * math.Pi / gamma // at q=0, (q/2)^beta*K_beta(q) ~ 1
        case _  => math.sqrt (ka * 2 * math.Pi / gamma * math.pow (q / 2.0, beta) )
      }
    }
    val sqrtSqij: Array[Array[Double]] = qij.map(_.map(q => sqrtSqijvals(qijmap(q))))

    // compute eta_q
    val etaq = sqrtSqij.zip(transformedField).map{case (sqr, uqr) => sqr.zip(uqr).map{case (sq, uq) => uq.multiply(sq)}}
    //println(etaq.map(_.map(_.getReal)).flatten.sum+" ; "+etaq.map(_.map(_.getImaginary)).flatten.sum)

    // inverse Fourier transform
    val eta = tr.mdfft(etaq, TransformType.INVERSE).asInstanceOf[Array[Array[Complex]]].map(_.map(d=>math.abs(d.getReal)))

    // unpad final result
    eta.dropRight(cflength-gridSize).map(_.dropRight(cflength-gridSize))
  }

}
