package org.openmole.spatialdata.application.swarmchemistry

import org.openmole.spatialdata.application.swarmchemistry.SwarmChemistry.CompetitionFunction
import org.openmole.spatialdata.grid.synthetic.ExpMixtureGridGenerator
import org.openmole.spatialdata.utils.math.Statistics

import java.util.StringTokenizer
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.util.control.Breaks._

case class SwarmChemistry(
                           mutationRateAtTransmission: Double = 0.1,
                           mutationRateAtNormalTime: Double = 0.05,
                           populationChangeMagnitude: Double = 0.8,
                           duplicationOrDeletionRatePerParameterSets: Double = 0.1,
                           randomAdditionRatePerRecipe: Double = 0.1,
                           pointMutationRatePerParameter: Double = 0.1,
                           pointMutationMagnitude: Double = 0.5,
                           majorityInteractionRadius: Double = 30.0,
                           recipeTransmissionRadius: Double = 10.0,
                           numberOfIndividualsMax: Int = 100,
                           neighborhoodRadiusMax: Double = 300.0,
                           normalSpeedMax: Double = 20.0,
                           maxSpeedMax: Double = 40.0,
                           c1Max: Double = 1.0,
                           c2Max: Double = 1.0,
                           c3Max: Double = 100.0,
                           c4Max: Double = 0.5,
                           c5Max: Double = 1.0,
                           worldWidth: Double = 5000.0,
                           worldHeight: Double = 5000.0,
                           worldXBound: Double = 150.0,
                           worldYBound: Double = 150.0,
                           spaceRasterWidth: Int = 500,
                           spaceRasterHeight: Int = 500,
                           initMaxSpeed: Double = 10.0,
                           endtime: Int = 10000,
                           competitionTypes: Seq[CompetitionFunction] = Seq(SwarmChemistry.Faster(), SwarmChemistry.Slower(), SwarmChemistry.Behind(), SwarmChemistry.Majority(), SwarmChemistry.MajorityRelative()),
                           zipfNCenters: Int = 10,
                           zipfMaxKernelRadius: Int = 50,
                           zipfCentersPopulationScaling: Double = 1.0,
                           zipfInsideThreshold: Double = 0.5
                         ) {



}


object SwarmChemistry {

  def runSwarmChemistry(initMode: String, spatialGenerator: String)(implicit model: SwarmChemistry, rng: Random): Unit = {
    import model._
    // initialise swarm
    initialisePopulation(initMode)
    // initialise space
    Space.initialiseSpace(spatialGenerator)

    println(s"Initialised: pop: ${swarmInBirthOrder.length} ; indicators: $indicators; world: ${spatialCompetition.head.head}")

    // run
    for(t <- 0 until endtime) {
      if(t%1000==0) println(s" === t = $t ; indicators = $indicators ; avg pos = (${swarmInBirthOrder.map(_.x).sum/swarmInBirthOrder.size.toDouble}, ${swarmInBirthOrder.map(_.y).sum/swarmInBirthOrder.size.toDouble})")
      simulateSwarmBehavior
      updateStates
    }
  }

  /**
   * c1 c2 c3 avg and sd
   * @return
   */
  def indicators: (Double, Double, Double, Double, Double, Double) = (
    swarmInBirthOrder.map(_.genome.c1).sum / swarmInBirthOrder.length.toDouble,
    Statistics.std(swarmInBirthOrder.map(_.genome.c1)),
    swarmInBirthOrder.map(_.genome.c2).sum / swarmInBirthOrder.length.toDouble,
    Statistics.std(swarmInBirthOrder.map(_.genome.c2)),
    swarmInBirthOrder.map(_.genome.c3).sum / swarmInBirthOrder.length.toDouble,
    Statistics.std(swarmInBirthOrder.map(_.genome.c3))
  )

  // mutable state of the simulation
  var swarmInBirthOrder: Array[SwarmIndividual] = Array.empty
  var swarmInXOrder: Array[SwarmIndividual] = Array.empty
  var swarmInYOrder: Array[SwarmIndividual] = Array.empty
  var spatialCompetition: Array[Array[CompetitionFunction]] = Array.empty

  trait CompetitionFunction
  case class Faster() extends CompetitionFunction
  case class Slower() extends CompetitionFunction
  case class Behind() extends CompetitionFunction
  case class Majority() extends CompetitionFunction
  case class MajorityRelative() extends CompetitionFunction

  def getCompetitionFunction(x: Double, y: Double)(implicit model: SwarmChemistry): CompetitionFunction = {
    val W = spatialCompetition(0).length.toDouble; val H = spatialCompetition.length.toDouble
    val i = ((x + model.worldWidth/2)*W/model.worldWidth).toInt
    val j = ((y + model.worldHeight/2)*H/model.worldHeight).toInt
    val ii: Int = math.min(math.max(i,0),W-1).toInt
    val jj: Int = math.min(math.max(j,0),H-1).toInt
    spatialCompetition(ii)(jj)
  }

  def initialisePopulation(initMode: String)(implicit model: SwarmChemistry, rng: Random): Unit = {

    val fixedParams = SwarmParameters(10, 0, 0, 0, 0, 0, 0, 1) // FIXME hardcoded

    val population = initMode match {
      // random pop with fixed majority
      case s: String if s.startsWith("random") =>
        val n = s.split(":")(1).toInt

        Seq.fill(n)(SwarmIndividual())++
          Seq.fill(model.numberOfIndividualsMax - n){
            SwarmIndividual((rng.nextDouble()-0.5)*model.worldWidth, (rng.nextDouble()-0.5)*model.worldHeight, 0.0, 0.0, fixedParams)
          }
      // recipe
      case r =>
        val recipe = Recipe(r)
        val tempPop = Recipe.createPopulation(recipe)
        tempPop++Seq.fill(model.numberOfIndividualsMax - tempPop.size){
          SwarmIndividual((rng.nextDouble()-0.5)*model.worldWidth, (rng.nextDouble()-0.5)*model.worldHeight, 0.0, 0.0, fixedParams)
        }
    }

    swarmInBirthOrder = population.toArray

    updateStates
  }

  def updateStates(implicit model: SwarmChemistry): Unit = {
    swarmInBirthOrder.foreach(SwarmIndividual.move(_))

    swarmInXOrder = swarmInBirthOrder.sortBy(_.x)
    swarmInYOrder = swarmInBirthOrder.sortBy(_.y)

    resetRanksInSwarm()
  }

  def resetRanksInSwarm(): Unit = {
    var tempSwarm: SwarmIndividual=null

    val xtoremove = new ArrayBuffer[SwarmIndividual]
    for (i <- swarmInXOrder.indices) {
      tempSwarm = swarmInXOrder(i)
      if (tempSwarm.rankInXOrder != -1) tempSwarm.rankInXOrder = i
      else xtoremove.addOne(tempSwarm)
    }
    swarmInXOrder = swarmInXOrder.filter(!xtoremove.contains(_))

    val ytoremove = new ArrayBuffer[SwarmIndividual]
    for (i <- swarmInYOrder.indices) {
      tempSwarm = swarmInYOrder(i)
      if (tempSwarm.rankInYOrder != -1) tempSwarm.rankInYOrder = i
      else ytoremove.addOne(tempSwarm)
    }
    swarmInYOrder = swarmInYOrder.filter(!ytoremove.contains(_))
  }


  /**
   * (competitionFunction: CompetitionFunction) -> not as main arg, but dependant on space
   *
   * @param model model
   * @param rng rng
   */
  def simulateSwarmBehavior(implicit model: SwarmChemistry, rng: Random): Unit = {

    import model._

    var tempSwarm: SwarmIndividual = null
    var tempSwarm2: SwarmIndividual = null
    var param: SwarmParameters = null
    var tempX: Double = 0.0
    var tempY: Double = 0.0
    var tempX2: Double = 0.0
    var tempY2: Double = 0.0
    var tempDX: Double = 0.0
    var tempDY: Double = 0.0
    var localCenterX: Double = 0.0
    var localCenterY: Double = 0.0
    var localDX: Double = 0.0
    var localDY: Double = 0.0
    var tempAx: Double = 0.0
    var tempAy: Double = 0.0
    var d: Double = 0.0
    var n: Int = 0
    var neighbors: Seq[SwarmIndividual] = Seq.empty

    val numberOfSwarm = swarmInBirthOrder.size

    for (i <- 0 until numberOfSwarm) {
      tempSwarm = swarmInBirthOrder(i)
      param = tempSwarm.genome
      tempX = tempSwarm.x
      tempY = tempSwarm.y

      var minRSquared: Double = initMaxSpeed * initMaxSpeed // FIXME hardcoded: same as init max speed?
      var tempRSquared: Double = 0.0
      var nearest: SwarmIndividual = null

      neighbors = neighborsOf(tempSwarm, model.recipeTransmissionRadius)
      n = neighbors.size
      for (j <- 0 until n) {
        tempSwarm2 = neighbors(j)
        if (tempSwarm2.recipe != null) {
          tempRSquared = (tempX - tempSwarm2.x) * (tempX - tempSwarm2.x) + (tempY - tempSwarm2.y) * (tempY - tempSwarm2.y)
          if (tempRSquared < minRSquared) {
            minRSquared = tempRSquared; nearest = tempSwarm2
          }
        }
      }

      //println(nearest)
      if (nearest != null) {
        if (tempSwarm.recipe != nearest.recipe) {
          //println("diff recipe")
          if (tempSwarm.recipe == null || losing(tempSwarm, nearest)) {
            if (rng.nextDouble() < mutationRateAtTransmission) tempSwarm.copy(recipe = Recipe.mutate(nearest.recipe))
            else tempSwarm = tempSwarm.copy(recipe = nearest.recipe)
            val newgenome = Recipe.randomlyPickParameters(tempSwarm.recipe)
            //println(s"transmission : delta c_i = ${newgenome - tempSwarm.genome}")
            tempSwarm = tempSwarm.copy(genome = newgenome)
          }
        }
      }

      neighbors = neighborsOf(tempSwarm, param.neighborhoodRadius)

      n = neighbors.size

      if (n == 0) {
        tempAx = rng.nextDouble() - 0.5
        tempAy = rng.nextDouble() - 0.5
      } else {
        localCenterX = 0.0
        localCenterY = 0.0
        localDX = 0.0
        localDY = 0.0
        for (j <- 0 until n) {
          tempSwarm2 = neighbors(j)
          localCenterX += tempSwarm2.x; localCenterY += tempSwarm2.y; localDX += tempSwarm2.dx; localDY += tempSwarm2.dy
        }

        localCenterX /= n.toDouble
        localCenterY /= n.toDouble
        localDX /= n.toDouble
        localDY /= n.toDouble

        if (tempSwarm.recipe != null) {

          if (rng.nextDouble() < 0.005) {
            tempSwarm = tempSwarm.copy(genome = Recipe.randomlyPickParameters(tempSwarm.recipe))
            param = tempSwarm.genome
          }
        }

        tempAx = 0.0; tempAy = 0.0
        tempAx += (localCenterX - tempX) * param.c1; tempAy += (localCenterY - tempY) * param.c1
        tempAx += (localDX - tempSwarm.dx) * param.c2; tempAy += (localDY - tempSwarm.dy) * param.c2

        for (j <- 0 until n) {
          tempSwarm2 = neighbors(j)
          tempX2 = tempSwarm2.x
          tempY2 = tempSwarm2.y
          d = (tempX - tempX2) * (tempX - tempX2) + (tempY - tempY2) * (tempY - tempY2)
          if (d == 0) d = 0.001
          tempAx += (tempX - tempX2) / d * param.c3
          tempAy += (tempY - tempY2) / d * param.c3
        }

        if (rng.nextDouble() < param.c4) {
          tempAx += (rng.nextDouble() - 0.5) * initMaxSpeed
          tempAy += (rng.nextDouble() - 0.5) * initMaxSpeed
        }
      }

      SwarmIndividual.accelerate(tempSwarm, tempAx, tempAy, param.maxSpeed)

      tempDX = tempSwarm.dx2; tempDY = tempSwarm.dy2
      d = math.sqrt(tempDX * tempDX + tempDY * tempDY)
      if (d == 0) d = 0.001
      SwarmIndividual.accelerate(tempSwarm, tempDX * (param.normalSpeed - d) / d * param.c5,
        tempDY * (param.normalSpeed - d) / d * param.c5,
        param.maxSpeed)

      if (rng.nextDouble() < mutationRateAtNormalTime)
        if (tempSwarm.recipe != null) tempSwarm = tempSwarm.copy(recipe = Recipe.mutate(tempSwarm.recipe))

      swarmInBirthOrder(i) = tempSwarm
    }
  }


  /**
   * compfunc: CompetitionFunction -> determined by position of agents -> average
   * @param defender defender
   * @param attacker attacker
   * @param model model
   * @param rng rng
   * @return
   */
  def losing(defender: SwarmIndividual, attacker: SwarmIndividual)(implicit model: SwarmChemistry, rng: Random): Boolean = {

    getCompetitionFunction((defender.x+attacker.x)/2, (defender.y+attacker.y)/2) match {
      case _: Faster => if (defender.dx * defender.dx + defender.dy * defender.dy > attacker.dx * attacker.dx + attacker.dy * attacker.dy) false else true
      case _: Slower => if (defender.dx * defender.dx + defender.dy * defender.dy < attacker.dx * attacker.dx + attacker.dy * attacker.dy) false else true
      case _: Behind =>
        val angle = 0.75 * Math.PI
        val threshold = Math.cos (angle)
        val ax = attacker.x - defender.x
        val ay = attacker.y - defender.y
        val bx = defender.dx
        val by = defender.dy
        if ((ax * bx + ay * by) > threshold * math.sqrt (ax * ax + ay * ay) * math.sqrt (bx * bx + by * by) ) false else true

      case _: Majority =>
        val interactionRadius = model.majorityInteractionRadius
        val defNeighbors = neighborsOf(defender, interactionRadius)
        val attNeighbors = neighborsOf(attacker, interactionRadius)
        var defNumber = 0
        for (j <- defNeighbors.indices) {
          if (defNeighbors(j).recipe == defender.recipe) defNumber = defNumber + 1
        }
        var attNumber = 0
        for (j <- attNeighbors.indices) {
          if (attNeighbors(j).recipe == attacker.recipe) attNumber =attNumber + 1
        }
        if (defNumber < attNumber) true else false

      case _: MajorityRelative =>
        val defNeighbors = neighborsOf(defender, math.max (model.majorityInteractionRadius, defender.genome.neighborhoodRadius))
        val attNeighbors = neighborsOf(attacker, math.max (model.majorityInteractionRadius, attacker.genome.neighborhoodRadius))
        var defNumber = 0.0
        for (j <- defNeighbors.indices) {
          if (defNeighbors(j).recipe == defender.recipe) defNumber += 1.0
          if (defNeighbors.size > 0.0) defNumber /= defNeighbors.size
        }
        var attNumber = 0.0
        for (j <- attNeighbors.indices) {
          if (attNeighbors(j).recipe == attacker.recipe) attNumber += 1.0
          if (attNeighbors.size > 0.0) attNumber /= attNeighbors.size
        }
        if (defNumber < attNumber) true else false

      // cases to add : "majority-stochastic" ; "majority-relative-stochastic" ; "recipe-length" ; "majority-and-recipe-length" ; "recipe-length-then-majority"
      case _ => false
    }
  }


  def neighborsOf(tempSwarm: SwarmIndividual, radius: Double): Seq[SwarmIndividual] = {
    val ngbs = new ArrayBuffer[SwarmIndividual]
    val tempX = tempSwarm.x
    val tempY = tempSwarm.y
    val neighborhoodRadiusSquared = radius * radius

    var tempSwarm2: SwarmIndividual = null

    val numberOfSwarm = swarmInBirthOrder.size

    val minX = tempX - radius
    val maxX = tempX + radius
    val minY = tempY - radius
    val maxY = tempY + radius
    var minRankInXOrder = tempSwarm.rankInXOrder
    var maxRankInXOrder = tempSwarm.rankInXOrder
    var minRankInYOrder = tempSwarm.rankInYOrder
    var maxRankInYOrder = tempSwarm.rankInYOrder

    //println(swarmInXOrder)

    breakable {for (j <- tempSwarm.rankInXOrder - 1 to 0 by -1) {
      if (swarmInXOrder(j).x >= minX) minRankInXOrder = j
      else break()
    }}

    breakable {for  (j <- tempSwarm.rankInXOrder + 1 until numberOfSwarm) {
      if (swarmInXOrder(j).x <= maxX) maxRankInXOrder = j
      else break()
    }}
    breakable {for (j <- tempSwarm.rankInYOrder - 1 to 0 by -1) {
      if (swarmInYOrder(j).y >= minY) minRankInYOrder = j
      else break()
    }}
    breakable {for (j <- tempSwarm.rankInYOrder + 1 until numberOfSwarm) {
      if (swarmInYOrder(j).y <= maxY) maxRankInYOrder = j
      else break()
    }}

    if (maxRankInXOrder - minRankInXOrder < maxRankInYOrder - minRankInYOrder) {
      for (j <- minRankInXOrder to maxRankInXOrder) {
        tempSwarm2 = swarmInXOrder(j)
        if (tempSwarm != tempSwarm2)
          if (tempSwarm2.rankInYOrder >= minRankInYOrder && tempSwarm2.rankInYOrder <= maxRankInYOrder) {
            if ((tempSwarm2.x - tempSwarm.x) * (tempSwarm2.x - tempSwarm.x) +
              (tempSwarm2.y - tempSwarm.y) * (tempSwarm2.y - tempSwarm.y) < neighborhoodRadiusSquared) ngbs.addOne(tempSwarm2)
          }
      }
    }
    else {
      for (j <- minRankInYOrder to maxRankInYOrder) {
        tempSwarm2 = swarmInYOrder(j)
        if (tempSwarm != tempSwarm2)
          if (tempSwarm2.rankInXOrder >= minRankInXOrder && tempSwarm2.rankInXOrder <= maxRankInXOrder) {
            if ((tempSwarm2.x - tempSwarm.x) * (tempSwarm2.x - tempSwarm.x) + (tempSwarm2.y - tempSwarm.y) * (tempSwarm2.y - tempSwarm.y) < neighborhoodRadiusSquared) ngbs.addOne(tempSwarm2)
          }
      }
    }

    ngbs.toSeq
  }


  case class SwarmParameters(
                              neighborhoodRadius: Double,
                              normalSpeed: Double,
                              maxSpeed: Double,
                              c1: Double,
                              c2: Double,
                              c3: Double,
                              c4: Double,
                              c5: Double
                            ) {
    def -(other: SwarmParameters): Double = math.abs(c1 - other.c1) + math.abs(c2 - other.c2)+ math.abs(c3 - other.c3)  //math.abs(neighborhoodRadius - other.neighborhoodRadius)+math.abs(normalSpeed - other.normalSpeed)+ math.abs(maxSpeed - other.maxSpeed)
  }

  object SwarmParameters {
    def apply()(implicit model: SwarmChemistry, rng: Random): SwarmParameters = {
      import model._
      SwarmParameters(
        neighborhoodRadius = rng.nextDouble() * neighborhoodRadiusMax,
        normalSpeed = rng.nextDouble() * normalSpeedMax,
        maxSpeed = rng.nextDouble() * maxSpeedMax,
        c1 = rng.nextDouble() * c1Max,
        c2 = rng.nextDouble() * c2Max,
        c3 = rng.nextDouble() * c3Max,
        c4 = rng.nextDouble() * c4Max,
        c5 = rng.nextDouble() * c5Max
      )
    }

    //def apply(p1: Double, p2: Double, p3: Double, p4: Double, p5: Double, p6: Double, p7: Double, p8: Double)(implicit model: SwarmChemistry): SwarmParameters = boundParameterValues(p1, p2, p3, p4, p5, p6, p7, p8)

    def boundParameterValues(p1: Double, p2: Double, p3: Double, p4: Double, p5: Double, p6: Double, p7: Double, p8: Double)(implicit model: SwarmChemistry): SwarmParameters={
      import model._
      SwarmParameters(
        neighborhoodRadius = if (p1<0.0) 0.0 else {if (p1> neighborhoodRadiusMax) neighborhoodRadiusMax else p1},
        normalSpeed =  if (p2<0.0) 0.0 else {if (p2> normalSpeedMax) normalSpeedMax else p2},
        maxSpeed =  if (p3<0.0) 0.0 else {if (p3> maxSpeedMax) maxSpeedMax else p3},
        c1 =  if (p4<0.0) 0.0 else {if (p4> c1Max) c1Max else p4},
        c2 =  if (p5<0.0) 0.0 else {if (p5> c2Max) c2Max else p5},
        c3 =  if (p6<0.0) 0.0 else {if (p6> c3Max) c3Max else p6},
        c4 =  if (p7<0.0) 0.0 else {if (p7> c4Max) c4Max else p7},
        c5 =  if (p8<0.0) 0.0 else {if (p8> c5Max) c5Max else p8},
      )
    }

    def displayColor(model: SwarmChemistry, params: SwarmParameters): (Double, Double, Double) = {
      import model._
      import params._
      (c1 / c1Max * 0.8, c2 / c2Max * 0.8, c3 / c3Max * 0.8)
    }

    def inducePointMutations(params: SwarmParameters, rate: Double, magnitude: Double)(implicit model: SwarmChemistry, rng: Random): SwarmParameters = {
      import model._
      import params._
      val p1 = if (rng.nextDouble() < rate) neighborhoodRadius + ((rng.nextDouble() - 0.5) * neighborhoodRadiusMax * magnitude) else neighborhoodRadius
      val p2 = if (rng.nextDouble() < rate) normalSpeed + ((rng.nextDouble() - 0.5) * normalSpeedMax * magnitude) else normalSpeed
      val p3 = if (rng.nextDouble() < rate) maxSpeed + ((rng.nextDouble() - 0.5) * maxSpeedMax * magnitude) else maxSpeed
      val p4 = if (rng.nextDouble() < rate) c1 + ((rng.nextDouble() - 0.5) *  c1Max * magnitude) else c1
      val p5 = if (rng.nextDouble() < rate) c2 + ((rng.nextDouble() - 0.5) *  c2Max * magnitude) else c2
      val p6 = if (rng.nextDouble() < rate) c3 + ((rng.nextDouble() - 0.5) *  c3Max * magnitude) else c3
      val p7 = if (rng.nextDouble() < rate) c4 + ((rng.nextDouble() - 0.5) *  c4Max * magnitude) else c4
      val p8 = if (rng.nextDouble() < rate) c5 + ((rng.nextDouble() - 0.5) *  c5Max * magnitude) else c5

      boundParameterValues(p1, p2, p3, p4, p5, p6, p7, p8)
    }


  }


  case class Recipe(
                     parameters: Seq[SwarmParameters],
                     popCounts: Seq[Int],
                     recipeText: String,
                     populationChangeMagnitude: Double = 0.8,
                     duplicationOrDeletionRatePerParameterSets: Double = 0.1,
                     randomAdditionRatePerRecipe: Double = 0.5,
                     pointMutationRatePerParameter: Double = 0.1,
                     pointMutationMagnitude: Double = 0.5
                   )

  object Recipe {
    val empty: Recipe = Recipe(Seq.empty, Seq.empty, "")

    def apply(text: String)(implicit model: SwarmChemistry): Recipe = {
      var ch: Char = 0
      var numberOfIngredients = 0
      var numberOfIndividuals = 0
      var neighborhoodRadius = .0
      var normalSpeed = .0
      var maxSpeed = .0
      var c1 = .0
      var c2 = .0
      var c3 = .0
      var c4 = .0
      var c5 = .0

      val recipeProcessed = new StringBuffer(text.length)
      for (i <- 0 until text.length) {
        ch = text.charAt(i)
        if ((ch >= '0' && ch <= '9') || (ch == '.')) recipeProcessed.append(ch)
        else if (recipeProcessed.length > 0) if (recipeProcessed.charAt(recipeProcessed.length - 1) != ' ') recipeProcessed.append(' ')
      }

      val st = new StringTokenizer(recipeProcessed.toString, " ")
      if (st.countTokens % 9 != 0) Recipe(Seq.empty, Seq.empty, "*** Formatting error ***\n" + text)
      numberOfIngredients = st.countTokens / 9
      if (numberOfIngredients == 0) Recipe(Seq.empty, Seq.empty, "*** No ingredients ***\n" + text)
      if (numberOfIngredients > model.numberOfIndividualsMax) numberOfIngredients = model.numberOfIndividualsMax
      val parameters = new ArrayBuffer[SwarmParameters]
      val popCounts = new ArrayBuffer[Int]
      try {
        for (_ <- 0 until numberOfIngredients) {
          numberOfIndividuals = st.nextToken.toInt
          if (numberOfIndividuals < 1) numberOfIndividuals = 1
          neighborhoodRadius = st.nextToken.toDouble
          normalSpeed = st.nextToken.toDouble
          maxSpeed = st.nextToken.toDouble
          c1 = st.nextToken.toDouble
          c2 = st.nextToken.toDouble
          c3 = st.nextToken.toDouble
          c4 = st.nextToken.toDouble
          c5 = st.nextToken.toDouble
          parameters.addOne(SwarmParameters.boundParameterValues(neighborhoodRadius, normalSpeed, maxSpeed, c1, c2, c3, c4, c5))
          popCounts.addOne(numberOfIndividuals)
        }
        Recipe(parameters.toSeq, popCounts.toSeq, text)
      } catch {
        case _: NumberFormatException => Recipe(Seq.empty, Seq.empty,  "*** Formatting error ***\n" + text)
      }
    }

    def apply(population: Seq[SwarmIndividual]): Recipe = {
      val parameters = new ArrayBuffer[SwarmParameters]
      val popCounts = new ArrayBuffer[Int]
      population.foreach { indiv =>
        if (!parameters.contains(indiv.genome)){
          parameters.addOne(indiv.genome)
          popCounts.addOne(1)
        } else{
          val j = parameters.indexOf(indiv.genome)
          popCounts(j) = popCounts(j) + 1
        }
      }
      Recipe(parameters.toSeq, popCounts.toSeq, recipeText(parameters.toSeq, popCounts.toSeq))
    }

    def apply(parameters: Iterable[SwarmParameters], popCounts: Iterable[Int]): Recipe = Recipe(parameters.toSeq, popCounts.toSeq, recipeText(parameters.toSeq, popCounts.toSeq))

    def recipeText(parameters: Seq[SwarmParameters], popCounts: Seq[Int]): String = {
      def shorten(d: Double): String = ((d * 100.0).round / 100.0).toString

      parameters.zip(popCounts).map{
        case (p, n) =>
          n.toString+ " * (" + shorten(p.neighborhoodRadius) + ", "+shorten(p.normalSpeed) + ", "+shorten(p.maxSpeed) + ", "+shorten(p.c1) + ", "+shorten(p.c2) + ", "+shorten(p.c3) + ", "+shorten(p.c4) + ", "+shorten(p.c5)+")"
      }.mkString("\n")
    }

    def recipeText(r: Recipe): String = recipeText(r.parameters, r.popCounts)

    def boundPopulationSize(recipe: Recipe)(implicit model: SwarmChemistry): Recipe = {
      val numberOfIngredients = recipe.parameters.size
      val totalPopulation = recipe.popCounts.sum
      val rescalingRatio = if ( totalPopulation > model.numberOfIndividualsMax)
        (model.numberOfIndividualsMax - numberOfIngredients).toDouble / (if (totalPopulation == numberOfIngredients) 1.0 else (totalPopulation - numberOfIngredients).toDouble)
      else 1.0
      recipe.copy(popCounts = recipe.popCounts.map{p => 1+ ((p - 1)*rescalingRatio ).toInt })
    }

    def createPopulation(recipe: Recipe)(implicit model: SwarmChemistry, rng: Random): Seq[SwarmIndividual] = {
      import model._
      recipe.parameters.zip(recipe.popCounts).flatMap {
        case (p, n) =>
          Seq.fill(n)(SwarmIndividual.apply((rng.nextDouble()-0.5)*worldWidth, (rng.nextDouble()-0.5)*worldHeight, (rng.nextDouble()-0.5)*initMaxSpeed, (rng.nextDouble()-0.5)*initMaxSpeed, p, recipe))
      }
    }

    def randomlyPickParameters(recipe: Recipe)(implicit rng: Random): SwarmParameters = {
      val totalPopulation = recipe.popCounts.sum
      val numberOfIngredients = recipe.parameters.size

      val r = (rng.nextDouble() * totalPopulation).toInt

      var j = 0
      for (i <- 0 until numberOfIngredients) {
        if (r >= j && r < j + recipe.popCounts(i)) return recipe.parameters(i)
        else j += recipe.popCounts(i)
      }
      recipe.parameters.head
    }

    def mutate(mutating: Recipe)(implicit model: SwarmChemistry, rng: Random): Recipe = {
      //println(s"mutating recipe $mutating")
      //var tempRecipe = mutating.copy(recipeText = recipeText(mutating))
      val parameters = new ArrayBuffer[SwarmParameters]; parameters.addAll(mutating.parameters)
      val popCounts = new ArrayBuffer[Int]; popCounts.addAll(mutating.popCounts)
      var numberOfIngredients: Int = parameters.size

      var j = 0
      while (j<numberOfIngredients){
        if (j < parameters.length) { // FIXME dirty fix
          if (rng.nextDouble() < model.duplicationOrDeletionRatePerParameterSets) {
            if (rng.nextDouble() < .5) { // Duplication

              if (j + 1 >= parameters.length) parameters.addOne(parameters(j)) else parameters(j + 1) = parameters(j)
              if (j + 1 >= popCounts.length) popCounts.addOne(popCounts(j)) else popCounts(j + 1) = popCounts(j)
              numberOfIngredients = numberOfIngredients + 1
              j = j + 1
            }
            else { // Deletion
              if (numberOfIngredients > 1) {
                parameters.remove(j)
                popCounts.remove(j)
                numberOfIngredients = numberOfIngredients - 1
                j = j - 1
              }
            }
          }
        }
        j = j + 1
      }

      if (rng.nextDouble() < model.randomAdditionRatePerRecipe) { // Addition
        parameters.addOne(SwarmParameters.apply())
        popCounts.addOne((rng.nextDouble() * model.numberOfIndividualsMax * 0.5).toInt + 1)
      }

      // Then Point Mutations
      val mutatedParams = parameters.map{p => SwarmParameters.inducePointMutations(p, model.pointMutationRatePerParameter, model.pointMutationMagnitude)}
      Recipe(mutatedParams, popCounts)
    }


  }



  case class SwarmIndividual(
                              genome: SwarmParameters,
                              recipe: Recipe = Recipe.empty
                            ) {
    var x: Double = 0.0
    var y: Double = 0.0
    var dx: Double = 0.0
    var dy: Double = 0.0
    var dx2: Double = 0.0
    var dy2: Double = 0.0
    var rankInXOrder: Int = 0
    var rankInYOrder: Int = 0
  }

  object SwarmIndividual {
    /**
     * random individual
     * @param  model model
     * @param rng rng
     * @return
     */
    def apply()(implicit model: SwarmChemistry, rng: Random): SwarmIndividual = {
      val randomParams = SwarmParameters.apply()
      val x = (rng.nextDouble()-0.5)*model.worldWidth
      val y = (rng.nextDouble()-0.5)*model.worldHeight
      SwarmIndividual(x,y,0.0,0.0, randomParams)
    }
    def apply(x: Double, y: Double, dx: Double, dy: Double, g: SwarmParameters): SwarmIndividual = {
      val indiv = SwarmIndividual(g); indiv.x = x; indiv.y = y; indiv.dx = dx; indiv.dy = dy; indiv.dx2 = dx; indiv.dy2 = dy
      val recipe = Recipe(Seq(indiv))
      indiv.copy(recipe = recipe)
    }
    def apply(x: Double, y: Double, dx: Double, dy: Double, g: SwarmParameters, r: Recipe): SwarmIndividual = {
      val indiv = SwarmIndividual(g, r); indiv.x = x; indiv.y = y; indiv.dx = dx; indiv.dy = dy; indiv.dx2 = dx; indiv.dy2 = dy
      indiv
    }

    def accelerate(indiv: SwarmIndividual, ax: Double, ay: Double, maxMove: Double): Unit = {
      indiv.dx2 = indiv.dx2 + ax; indiv.dy2 = indiv.dy2 + ay
      val d = indiv.dx2*indiv.dx2 + indiv.dy2*indiv.dy2
      if (d > maxMove * maxMove) {
        val normalizationFactor = maxMove / math.sqrt(d)
        indiv.dx2 = indiv.dx2*normalizationFactor
        indiv.dy2 = indiv.dy2*normalizationFactor
      }
    }

    def move(indiv: SwarmIndividual)(implicit model: SwarmChemistry): Unit = {
      import model._
      indiv.dx = indiv.dx2
      indiv.dy = indiv.dy2
      indiv.x = indiv.x + indiv.dx
      indiv.y = indiv.y + indiv.dy
      // torus world
      if (indiv.x > worldWidth/2.0 + worldXBound) indiv.x = indiv.x - worldWidth
      if (indiv.x < -worldWidth/2.0 - worldXBound) indiv.x = indiv.x + worldWidth
      if (indiv.y > worldHeight/2.0 + worldYBound) indiv.y = indiv.y - worldHeight
      if (indiv.y < -worldHeight/2.0 - worldYBound) indiv.y = indiv.y + worldHeight
    }

    def displayColor(model: SwarmChemistry, indiv: SwarmIndividual): (Double, Double, Double) = SwarmParameters.displayColor(model, indiv.genome)

  }


  object Space {


    /**
     * random among global list of compet types; for later: compare different types of compet
     * @param mode mode
     * @param model model
     * @param rng rng
     */
    def initialiseSpace(mode: String)(implicit model: SwarmChemistry, rng: Random): Unit = {
      spatialCompetition = mode match {
        case "random" => Array.fill(model.spaceRasterWidth,model.spaceRasterHeight){rng.shuffle(model.competitionTypes).head}
        case "uniform" =>
          val compet = rng.shuffle(model.competitionTypes).head
          Array.fill(model.spaceRasterWidth,model.spaceRasterHeight)(compet)

        case "split" => // simple split
          val xsplit = (rng.nextDouble()-0.5)*model.worldWidth*0.8
          val ysplit = (rng.nextDouble()-0.5)*model.worldHeight*0.8
          val quadrants = Seq.fill(4){rng.shuffle(model.competitionTypes).head}
          Array.tabulate(model.spaceRasterWidth,model.spaceRasterHeight){case (i,j) =>
            val x = (i.toDouble/model.spaceRasterWidth.toDouble - 0.5)*model.worldWidth
            val y = (j.toDouble/model.spaceRasterHeight.toDouble - 0.5)*model.worldHeight
            if (x < xsplit && y < ysplit) quadrants.head
            if (x < xsplit && y > ysplit) quadrants(1)
            if (x > xsplit && y < ysplit) quadrants(2)
            if (x > xsplit && y > ysplit) quadrants(3)
            quadrants.head
          }

        case "zipf" => // hierarchical structure
          val kernelSizes = (1 to model.zipfNCenters).map(i => model.zipfMaxKernelRadius/math.pow(i, model.zipfCentersPopulationScaling))
          val pr: Array[Array[Double]] =
            ExpMixtureGridGenerator(Right((model.spaceRasterWidth, model.spaceRasterHeight)),
              model.zipfNCenters, 1.0, kernelSizes).generateGrid
          val m = pr.flatten.max(Ordering.Double.TotalOrdering)
          val prnorm = pr.map(_.map(_/m))
          val io = (rng.shuffle(model.competitionTypes).head, rng.shuffle(model.competitionTypes).head)
          Array.tabulate(model.spaceRasterWidth,model.spaceRasterHeight) { case (i, j) =>
            if (prnorm(i)(j)> model.zipfInsideThreshold) io._1 else io._2
          }
      }
    }
  }




}

