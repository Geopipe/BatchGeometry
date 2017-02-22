package com.geopipe.modeltools

import scala.xml._

import com.geopipe.profiling.TicToc.{tic,toc}
import com.geopipe.xml.SubtreeRewriter

abstract class PipelineRuleStage[+T] extends PartialFunction[Node,Node] {
	def sideChannel:Map[Class[_],T] = Map()
	protected def impl:PartialFunction[Node, Node]
	override def isDefinedAt(n: Node):Boolean = impl.isDefinedAt(n)
	override def apply(n: Node): Node = impl(n)
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
				val ret = SubtreeRewriter(stage)(collada)
				toc(s"rewriter execution at stage $i")
				i += 1
				(ret, sideChannel ++ stage.sideChannel)
		}
	}
}