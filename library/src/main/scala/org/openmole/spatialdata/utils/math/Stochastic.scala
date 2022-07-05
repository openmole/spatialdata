package org.openmole.spatialdata.utils.math

import scala.util.Random
import org.apache.commons.rng.sampling.ListSampler
import org.apache.commons.rng.simple.RandomSource

import scala.jdk.CollectionConverters._

object Stochastic {


  sealed trait Distribution {
    def rng: Random
    def draw: Double
  }

  case class NormalDistribution(mu: Double, sigma: Double)(implicit localRng: Random) extends Distribution {
    override def rng: Random = localRng
    override def draw: Double = mu + sigma*rng.nextGaussian()
  }

  case class LogNormalDistribution(mu: Double, sigma: Double)(implicit localRng: Random) extends Distribution {
    override def rng: Random = localRng
    override def draw: Double = math.exp(mu + sigma*rng.nextGaussian())
  }
  object LogNormalDistribution {
    def fromMoments(average: Double, std: Double)(implicit localRng: Random): LogNormalDistribution = {
      assert(average>0,"Average of a log-normal should be strictly positive")
      assert(std>0,"Std of a log-normal should be strictly positive")
      assert(math.log(std)<math.log(average)-0.5,"Std of log normal too low regarding average")
      LogNormalDistribution(
        mu = 2 * math.log(average) - math.log(std) - 0.5,
        sigma = math.sqrt(1 - 2*(math.log(average) - math.log(std)))
      )
    }
  }

  /**
    * Draw a random variable with a poisson distribution
    * @param lambda lambda
    * @param localRng rng
    * @return
    */
  case class PoissonDistribution(lambda: Double)(implicit localRng: Random) extends Distribution {
    override def rng: Random = localRng

    override def draw: Double = {
      val el = math.exp(-1.0*lambda)
      def prec(p: Double,k: Int): Int = p match {
        case pp if pp > el => prec(pp*rng.nextDouble(),k+1)
        case _ => k - 1
      }
      prec(1.0,0).toDouble
    }
  }


  /**
    * The probability function is assumed to effectively define a probability measure on elements of sampled
    * @param sampled sampled iterable
    * @param probability probability function
    * @param rng rng
    * @tparam T type of iterable
    * @return
    */
  def sampleOneBy[T](sampled: Iterable[T], probability: T => Double)(implicit rng: Random): T = {
    def f(s: (Iterable[T],T,Double,Double)): (Iterable[T], T, Double, Double) = {//s._1.size match{
    //case 0 => (s._1,)
    // case _ =>
     // println(s._3 + probability (s._1.head));println(s._3+" ; "+s._4)
        (s._1.tail, s._1.head, s._3 + probability (s._1.head), s._4)
    }
    f(Iterator.iterate((sampled,sampled.head,0.0.toDouble,rng.nextDouble()))(f).takeWhile(s => s._3 < s._4&&s._1.nonEmpty).toSeq.last)._2
  }

  def sampleWithReplacementBy[T](sampled: Iterable[T], probability: T => Double, samples: Int)(implicit rng: Random): Vector[T] =
    (0 until samples).map(_ => sampleOneBy(sampled, probability)).toVector

  /**
    * rq: fails on a Set -> be less general than Iterable?
    * @param sampled sampled
    * @param samples number of samples
    * @param rng rng
    * @tparam T type
    * @return
    */
  def sampleWithReplacement[T](sampled: Iterable[T], samples: Int)(implicit rng: Random): Vector[T] =
    sampleWithReplacementBy[T](sampled,_ => 1.0 / sampled.size.toDouble, samples)

  /**
    * Sample without replacement
    *   ! this function is a disaster for large iterables
    * @param sampled sampled
    * @param probability proba function
    * @param samples samples
    * @param rng rng
    * @tparam T type
    * @return
    */
  def sampleWithoutReplacementBy[T](sampled: Iterable[T], probability: T => Double, samples: Int)(implicit rng: Random): Vector[T] = {
    assert(samples <= sampled.size,"Can not sample more than vector size : "+samples+" / "+sampled.size)
    Iterator.iterate((sampled, Vector.empty[T])) { case (rest, res) =>
      val totproba = rest.map(probability(_)).sum // ! not efficient, could be computed from the previously sampled proba
      val normalizedProba = rest.toSeq.map(probability(_) / totproba)
      val sample = sampleOneBy[((T, Int), Double)](rest.toSeq.zipWithIndex.zip(normalizedProba), _._2)
      (rest.toSeq.zipWithIndex.filter(_._2 != sample._1._2).map(_._1), Vector(sample._1._1) ++ res)
    }.take(samples).toSeq.last._2
  }

  def sampleWithoutReplacement[T](sampled: Iterable[T], samples: Int)(implicit rng: Random): Vector[T] = {
    if (samples<=0) Vector.empty else
    ListSampler.sample(RandomSource.create(RandomSource.MT, rng.nextLong()), sampled.toList.asJava, samples).asScala.toVector
  }

  //sampleWithoutReplacementBy[T](sampled,_ => 1.0 / sampled.size.toDouble, samples)

}
