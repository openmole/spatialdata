package org.openmole.spatialdata.application.quant

import org.openmole.spatialdata.utils.math.Matrix
import org.openmole.spatialdata.vector.SpatialField


case class QUANT(
                observedFlows: Seq[Matrix],
                distances: Seq[Matrix],
                originValues: SpatialField,
                destinationValues: SpatialField
                )



object QUANT {

}
