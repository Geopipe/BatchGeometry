package com.geopipe.modeltools

import scala.xml._
import scala.xml.transform._

import org.json4s._

import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

import java.io.FileWriter
import org.apache.commons.io.FilenameUtils


object BatchGeometry extends TransformerPipeline(List(new UniqueImagesRewriter(_), new UniqueEffectsRewriter(_), new BatchedGeometryRewriter(_))) {
	def processInputFiles(filesToProcess:RDD[(String,String)], outputSuffix:String, jsonSuffix:String) = {
		filesToProcess.foreach{
			case (path, content) =>
				val (outputXML, sideChannel) = applyPipeline(XML.loadString(content))
				val outputJSON = sideChannel.getOrElse(classOf[BatchedGeometryRewriter],JNothing)
				val outputBasePath = FilenameUtils.removeExtension(path).stripPrefix("file:")
				
				val outputDst = outputBasePath + outputSuffix
				val jsonDst = outputBasePath + jsonSuffix
				
				XML.save(outputDst, outputXML, enc="utf-8", xmlDecl = true)
				native.Serialization.write(outputJSON, new FileWriter(jsonDst))(native.Serialization.formats(NoTypeHints))
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