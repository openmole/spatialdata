package org.openmole.spatialdata.model.urbandynamics

/**
  * A macroscopic urban dynamic model
  */
trait MacroModel {
  def run: MacroResult
}
