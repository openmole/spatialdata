package org.openmole.spatialdata.utils.osm.parser.xml.instantiated

import java.util

import org.openmole.spatialdata.utils.osm.domain.{Node, Relation, Way}


/**
  * @author kalle
  * @since 2013-05-02 03:20
  */
class InstantiatedOsmXmlParserDelta {
  private var createdNodes: util.Set[Node] = new util.HashSet[Node]
  private var modifiedNodes: util.Set[Node] = new util.HashSet[Node]
  private var deletedNodes: util.Set[Node] = new util.HashSet[Node]
  private var createdWays: util.Set[Way] = new util.HashSet[Way]
  private var modifiedWays: util.Set[Way] = new util.HashSet[Way]
  private var deletedWays: util.Set[Way] = new util.HashSet[Way]
  private var createdRelations: util.Set[Relation] = new util.HashSet[Relation]
  private var modifiedRelations: util.Set[Relation] = new util.HashSet[Relation]
  private var deletedRelations: util.Set[Relation] = new util.HashSet[Relation]

  def getCreatedNodes = createdNodes

  def setCreatedNodes(createdNodes: util.Set[Node]) = {
    this.createdNodes = createdNodes
  }

  def getModifiedNodes = modifiedNodes

  def setModifiedNodes(modifiedNodes: util.Set[Node]) = {
    this.modifiedNodes = modifiedNodes
  }

  def getDeletedNodes = deletedNodes

  def setDeletedNodes(deletedNodes: util.Set[Node]) = {
    this.deletedNodes = deletedNodes
  }

  def getCreatedWays = createdWays

  def setCreatedWays(createdWays: util.Set[Way]) = {
    this.createdWays = createdWays
  }

  def getModifiedWays = modifiedWays

  def setModifiedWays(modifiedWays: util.Set[Way]) = {
    this.modifiedWays = modifiedWays
  }

  def getDeletedWays = deletedWays

  def setDeletedWays(deletedWays: util.Set[Way]) = {
    this.deletedWays = deletedWays
  }

  def getCreatedRelations = createdRelations

  def setCreatedRelations(createdRelations: util.Set[Relation]) = {
    this.createdRelations = createdRelations
  }

  def getModifiedRelations = modifiedRelations

  def setModifiedRelations(modifiedRelations: util.Set[Relation]) = {
    this.modifiedRelations = modifiedRelations
  }

  def getDeletedRelations = deletedRelations

  def setDeletedRelations(deletedRelations: util.Set[Relation]) = {
    this.deletedRelations = deletedRelations
  }
}
