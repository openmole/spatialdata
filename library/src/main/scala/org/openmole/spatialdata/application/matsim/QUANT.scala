package org.openmole.spatialdata.application.matsim

import org.locationtech.jts.geom
import org.openmole.spatialdata.application.matsim.SpenserSynthPop.Individual
import org.openmole.spatialdata.application.quant.QUANTMultiMode
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.io.{Binary, CSV}
import org.openmole.spatialdata.utils.math.SparseMatrix
import org.openmole.spatialdata.vector.Polygons

import scala.collection.mutable
import scala.util.Random

/**
  * QUANT implementation for integration into MATSim synthetic population generator
  */
object QUANT {

  val QUANTMSOAFileName: String = "EWS_ZoneCodes.csv"
  val QUANTMSOAKey: String = "areakey"
  val QUANTMSOAIndex: String = "zonei"
  val QUANTFlowsFiles: Array[String] = (1 to 3).map(i => "TObs_"+i+".bin").toArray
  val QUANTDistancesFiles: Array[String] = Seq("roads", "gbrail", "bus").map(s => "dis_"+s+"_min.bin").toArray

  implicit val sparseMatrixImplementation: SparseMatrix.SparseMatrixImplementation = SparseMatrix.SparseBreeze()


  /**
    * Generate job location sampling empirical flows
    *  (not QUANT in proper sense but uses the same data)
    *
    *  Note: use directly individual location at the MSOA level provided in its data - precise home location
    *   does not matter for the sampling -> the correspondance OA -> MSOA is not necessary
    *
    *  ! some MSOA from individuals are missed when sampling from the area: different versions? more constraining sampling?
    *
    * @param population jobless population
    * @param QUANTDataDir data dir
    * @param area area
    * @param msoas msoas (already assumed filtered within area)
    * @return
    */
  def empiricalSamplingJobLocationPopulation(population: SpenserSynthPop, QUANTDataDir: String, area: geom.Geometry, msoas: Polygons, oas: Polygons)(implicit rng: Random): SpenserSynthPop = {
    // filter msoa indices
    val (msoaIndices, msoaIndexMap, msoaNameMap, msoaCoords, _) = getMSOAIndices(QUANTDataDir, msoas, oas)

    //println(oaMap.keys)
    //println(population.individuals.map(_.oaCode))

    // load and filter flows
    val sparseFlows = loadAndFilterMatrices(QUANTFlowsFiles.map(f => QUANTDataDir+f), msoaIndices.toArray)

    // sample filtered flows: not optimized, go through 3 mode rows - should sort individuals by draw
    // and advance simultaneously in indiv tab/probas
    //val groupedIndivs = population.individuals.filter(i=>oaMap.contains(i.oaCode)).map(i => (i,oaMap(i.oaCode))).groupBy(_._2)
    val groupedIndivs = population.individuals.map(i => (i,i.msoaCode)).groupBy(_._2)
    //utils.log(groupedIndivs.toString())
    val locatedindivs = groupedIndivs.map { case (msoaCode, individuals) =>
      if (msoaIndexMap.contains(msoaCode)) {
        val rows = sparseFlows.map(_.getRow(msoaIndexMap(msoaCode)).values.flatten) // ! matrices are already sampled
        val rowsums = rows.map(_.sum)
        val probas = rows.zip(rowsums).map { case (r, s) => r.map(_ / s) }
        val indivmode = individuals.map(_ => rng.nextInt(3))
        val indivdraw = individuals.map(_ => rng.nextDouble())
        val indivs: Seq[(Individual, Int, Double)] = individuals.zip(indivmode).zip(indivdraw).map { case ((i, m), r) => (i._1, m, r) }
        val indivDestMap = new mutable.HashMap[Individual, (Double, Double)]()
        //println(probas.map(_.toSeq).toSeq)
        //println(indivmode)
        //println(indivdraw)
        for {
          i <- probas(0).indices
        } {
          indivs.foreach { ind => if (probas(ind._2)(i) < ind._3 && (!indivDestMap.contains(ind._1))) indivDestMap.put(ind._1, msoaCoords(msoaNameMap(i))) }
        }
        val locatedindivs = indivDestMap.toSeq.map { case (i, loc) => i.copy(workLocation = loc) }
        utils.log(s"Located jobs from MSOA $msoaCode : ${locatedindivs.size}")
        locatedindivs
      } else Seq.empty
    }.reduce(utils.concat[Individual])

    SpenserSynthPop(locatedindivs, population.households)
  }


  /**
    * Calibrate the QUANT model and generate job locations according to parameters
    *
    * @param population jobless population
    * @param QUANTDataDir data dir
    * @param area area
    * @param msoas msoas
    * @return
    */
  def gravityJobLocationPopulation(population: SpenserSynthPop, QUANTDataDir: String, area: geom.Geometry, msoas: Polygons, oas: Polygons): SpenserSynthPop = {
    val (msoaIndices, msoaIndexMap, msoaNameMap, msoaCoords, oaMap) = getMSOAIndices(QUANTDataDir, msoas, oas)
    val sparseFlows = loadAndFilterMatrices(QUANTFlowsFiles.map(f => QUANTDataDir+f), msoaIndices.toArray)
    val sparseDistances = loadAndFilterMatrices(QUANTDistancesFiles.map(f => QUANTDataDir+f), msoaIndices.toArray)

    // calibrate QUANT model
    val modelfitted = QUANTMultiMode.QUANTMultiMode(sparseFlows,sparseDistances).fitted

    utils.log(s"fitted params = ${modelfitted.fittedParams.toSeq}")

    SpenserSynthPop(Seq.empty, Seq.empty)
  }


  /**
    * get MSOA indices: load QUANT csv
    * @param QUANTDataDir data dir
    * @param msoas msoas
    * @return
    */
  def getMSOAIndices(QUANTDataDir: String, msoas: Polygons, oas: Polygons): (Seq[Int], Map[String, Int], Map[Int, String], Map[String, (Double, Double)], Map[String, String]) = {
    val msoaRawData = CSV.readCSV(filename = QUANTDataDir+QUANTMSOAFileName)
    val msoaIndexMap: Map[String, Int] = msoaRawData(QUANTMSOAKey).zip(msoaRawData(QUANTMSOAIndex).map(_.toInt)).toMap
    val msoaNames = msoas.attributes.flatMap(_.get(Population.msoaID)).map(_.toString)
    val msoaIndices = msoaNames.map(id => msoaIndexMap(id))
    utils.log(s"   Sampling MSOA indices: $msoaIndices")
    // construct OA -> MSOA overlay
    val msoanamed = msoas.polygons.zip(msoas.attributes.map(_.getOrElse(Population.msoaID,"NA").toString))
    val msoaCoords = msoanamed.map{case (p,name) => (name, (p.getCentroid.getX, p.getCentroid.getY))}.toMap
    val oaMap = oas.polygons.zip(oas.attributes.map(_.getOrElse(Population.oaID,"NA").toString)).flatMap{case (oa,id)=>
      val potmsoaa = msoanamed.filter(m => m._1.contains(oa)||m._1.intersects(oa))
      if (potmsoaa.nonEmpty) Some((id,potmsoaa.head._2)) //should not be empty
      else {
        utils.log(s"OA outside MSOAa: $id")
        //(id,msoanamed.head._2)
        None
      } // random msoa
    }.toMap

    val filteredMsoaIndexMap = msoaNames.zipWithIndex.toMap
    val filteredMsoaNameMap = filteredMsoaIndexMap.map(_.swap)

    (msoaIndices, filteredMsoaIndexMap, filteredMsoaNameMap, msoaCoords, oaMap)
  }

    /**
    * ! unsafe operation here, assumes SparseMatrix submatrix is also sparse - this depends on the implementation
    *   -> should add a sparseSubMat primitive specific to SparseMatrix
    *
    * @param files files
    * @param msoaIndices filtered MSOAs
    * @return
    */
  def loadAndFilterMatrices(files: Array[String], msoaIndices: Array[Int]): Array[SparseMatrix] = {
    val matrices = files.map(Binary.readBinary[SparseMatrix])
    matrices.map{m => m.getSubmat(msoaIndices, msoaIndices).asInstanceOf[SparseMatrix]}
  }



}
