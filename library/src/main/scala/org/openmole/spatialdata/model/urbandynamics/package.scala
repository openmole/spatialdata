package org.openmole.spatialdata.model


/**
  *
  * Urban dynamics model correspond to agent-based model at the macroscopic scale for systems of cities.
  * They can be used to generate synthetic systems of cities or to simulate dynamics of real systems.
  *
  * The class of models implemented here is inheriting from evolutionary urban theory simulation models, see:
  *  Pumain, D. (1997). Pour une théorie évolutive des villes. L'Espace géographique, 119-134.
  *  Pumain, D. (2018). An evolutionary theory of urban systems. In International and transnational perspectives on urban systems (pp. 3-18). Springer, Singapore.
  * for the theoretical background, and
  * Sanders, L., Pumain, D., Mathian, H., Guérin-Pace, F., & Bura, S. (1997). SIMPOP: a multiagent system for the study of urbanism. Environment and Planning B: Planning and design, 24(2), 287-305.
  * for the first model
  * Pumain, D. (2012). Multi-agent system modelling for urban systems: The series of simpop models. In Agent-based models of geographical systems (pp. 721-738). Springer, Dordrecht.
  * for a synthesis of Simpop models.
  *
  * Implemented models include diverse processes:
  *  - physical transportation network and coevolution
  *      Raimbault, J. (2020). Indirect evidence of network effects in a system of cities. Environment and Planning B: Urban Analytics and City Science, 47(1), 138-155.
  *      Raimbault, J. (2018). Modeling the co-evolution of cities and networks. arXiv preprint arXiv:1804.09430.
  *  - diffusion of innovation
  *      Favaro, J. M., & Pumain, D. (2011). Gibrat Revisited: An Urban Growth Model Incorporating Spatial Interaction and Innovation Cycles. Geographical Analysis, 43(3), 261-286.
  *      Raimbault, J. (2020). A model of urban evolution based on innovation diffusion. Artifical Life 2020 Proceedings.
  */
package object urbandynamics
