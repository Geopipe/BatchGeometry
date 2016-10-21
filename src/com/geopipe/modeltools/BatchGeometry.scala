package com.geopipe.modeltools

import scala.xml._
import scala.xml.transform._

object BatchGeometry {
	def makeImagesUnique(collada:Node):Node = {
		val transformer = new RuleTransformer(new UniqueImagesRewriter(collada))
		transformer(collada)
	}
	
	def makeEffectsUnique(collada:Node):Node = {
		val transformer = new RuleTransformer(new UniqueEffectsRewriter(collada))
		transformer(collada)
	}
	
	def main(args:Array[String]):Unit = {
		val collada = XML.loadFile(args(0))
		val withUniqueImages = makeImagesUnique(collada)
		val withUniqueEffects = makeEffectsUnique(withUniqueImages)
		Console.println(collada)
	}
}