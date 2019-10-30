package org.openmole.spatialdata.test

object Test extends App {

  // Coupled network / grid
  TestCoupled.testWeakCouplingNodePositions()
  //TestCoupled.testRandom()

  // Network algorithms
  //TestNetwork.testPlanarization()

  // OSM
  //TestOSM.testBuildingExtractor
  //TestOSM.testOSMGridSampling

  // Database
  //TestDatabase.testPostgis
  //TestDatabase.testMongo

  // Indicators
  //TestIndicators.testSpatialIndics
  //TestIndicators.testMoran
  //TestIndicators.testStats
  //TestIndicators.testFFTConvolution

  // Synthetic grids
  //TestSynthetic.testRandomGrids
  //TestSynthetic.testBlocksGrid
  //TestSynthetic.testPercolationGrid
  //TestSynthetic.testGeneratorLauncher
  //TestSynthetic.testGeneratorCalibration


  // Utils
  //TestUtils.testSampling
  //TestUtils.testCSV
  //TestPNG.testPNG()
  //TestPNG.testOSMGridSampling()

}
