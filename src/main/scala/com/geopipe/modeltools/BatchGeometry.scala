package com.geopipe.modeltools

import scala.xml._
import scala.xml.transform._

import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

class BatchedGeometryRewriter(collada:Node) extends RewriteRule {
	
}

object BatchGeometry {
	def makeImagesUnique(collada:Node):Node = {
		val transformer = new RuleTransformer(new UniqueImagesRewriter(collada))
		transformer(collada)
	}
	
	def makeEffectsUnique(collada:Node):Node = {
		val transformer = new RuleTransformer(new UniqueEffectsRewriter(collada))
		transformer(collada)
	}
	
	def processInputFiles(filesToProcess:RDD[(String,String)], outputSuffix:String, jsonSuffix:String) = {
		filesToProcess.foreach{
			case (path, content) => 
				val collada = XML.loadString(content)
				val withUniqueImages = makeImagesUnique(collada)
				val withUniqueEffects = makeEffectsUnique(withUniqueImages)
				Console.println(withUniqueEffects)
		}
	}
	
	def main(args:Array[String]):Unit = {
		args match {
			case Array(dirPattern, outputSuffix, jsonSuffix) =>
				processInputFiles(SparkContext.getOrCreate.wholeTextFiles(dirPattern), outputSuffix, jsonSuffix)
			case _ => 
				Console.err.println("BatchGeometry takes 3 arguments: dirPattern, outputSuffix, jsonSuffix")
				System.exit(-1)
		}
	}
}