package com.geopipe.modeltools

import scala.xml._
import scala.xml.transform._

import org.json4s._

class BatchedGeometryRewriter(collada:Node) extends PipelineRuleStage[JValue] {
	override def sideChannel = Map()
}