package com.geopipe.modeltools

import scala.xml._
import scala.xml.transform._

abstract class PipelineRuleStage[+T] extends RewriteRule {
	def sideChannel:Map[Class[_],T]
}

class TransformerPipeline[+T](pipeline:Seq[Node => PipelineRuleStage[T]]) {
	def applyPipeline(collada:Node):(Node,Map[Class[_],T]) = {
		pipeline.foldLeft((collada,Map[Class[_],T]())){ 
			case ((collada,sideChannel), rewriterAlloc) => 
				val stage = rewriterAlloc(collada)
			((new RuleTransformer(stage))(collada), sideChannel ++ stage.sideChannel)
		}
	}
}