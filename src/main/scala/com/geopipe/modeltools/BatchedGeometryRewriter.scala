package com.geopipe.modeltools

import scala.xml._
import scala.xml.transform._

import org.json4s._

class BatchedGeometryRewriter(collada:Node) extends PipelineRuleStage[JValue] {
	val indexedSceneNodes = MiscHelpers.retrieveSceneNodes(collada).zipWithIndex
	val metaData = JObject(indexedSceneNodes.map{
		case (node, i) => 
			val metaDataRoot = node \ "extra" \ "technique"
			(i.toString, JObject(metaDataRoot.collectFirst{case e:Elem if e\@"profile" == "geopipe_Metadata" => e}.map{
				e => (e \ "param").collect{
					case p:Elem => ((p\@"semantic"), JString(p.text))
				}
			}.getOrElse(Nil).toList))
	}.toList)
	
	
	override def sideChannel =	Map(classOf[BatchedGeometryRewriter] -> metaData)
}