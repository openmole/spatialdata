package org.openmole.spatialdata.scalatest.grid

import org.openmole.spatialdata.scalatest.grid.synthetic.GridSyntheticSuite
import org.scalatest.Suites

class GridSuite extends Suites (
  new GridSpec,
  new GridSyntheticSuite
)
