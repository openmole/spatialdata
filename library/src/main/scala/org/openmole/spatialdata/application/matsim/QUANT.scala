package org.openmole.spatialdata.application.matsim

import org.openmole.spatialdata.utils.math.SparseMatrix

/**
  * QUANT implementation for integration into MATSim synthetic population generator
  */
object QUANT {



  
  /**
    * ! unsafe operation here, assumes SparseMatrix submatrix is also sparse - this depends on the implementation
    *   -> should add a sparseSubMat primitive specific to SparseMatrix
    *
    * @param matrices matrices
    * @param msoaIndices filtered MSOAs
    * @return
    */
  def filterMatrices(matrices: Array[SparseMatrix], msoaIndices: Array[Int]): Array[SparseMatrix] =
    matrices.map{m => m.getSubmat(msoaIndices, msoaIndices).asInstanceOf[SparseMatrix]}


}
