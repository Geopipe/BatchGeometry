package com.geopipe.modeltools

import scala.xml._
import scala.xml.transform._

import com.geopipe.profiling.TicToc.{tic,toc}

abstract class PipelineRuleStage[+T] extends RewriteRule {
	def sideChannel:Map[Class[_],T]
}

class TransformerPipeline[+T](pipeline:Seq[Node => PipelineRuleStage[T]]) {
	def applyPipeline(collada:Node):(Node,Map[Class[_],T]) = {
		var i = 0
		pipeline.foldLeft((collada,Map[Class[_],T]())){ 
			case ((collada,sideChannel), rewriterAlloc) => 
				tic
				val stage = rewriterAlloc(collada)
				toc(s"rewriterAlloc at stage $i")
				
				tic
				val ret = (new RuleTransformer(stage))(collada)
				toc(s"rewriter execution at stage $i")
				i += 1
				(ret, sideChannel ++ stage.sideChannel)
		}
	}
}