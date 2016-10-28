package com.geopipe.modeltools

import scala.xml._
import scala.xml.transform._
import org.apache.spark._

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
		val sc = SparkContext.getOrCreate
		val dirPattern = args(0)
		val filesToProcess = sc.wholeTextFiles(dirPattern)
		filesToProcess.foreach{
			case (path, content) => 
				val collada = XML.loadString(content)
				val withUniqueImages = makeImagesUnique(collada)
				val withUniqueEffects = makeEffectsUnique(withUniqueImages)
				Console.println(withUniqueEffects)
		}
	}
}