package org.openmole.spatialdata.model.urbandynamics

import org.openmole.spatialdata.utils.math.Matrix

trait RealDataModel {
  def realPopulations: Matrix
}
