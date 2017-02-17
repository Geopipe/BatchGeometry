package com.geopipe.modeltools

import scala.xml._
import scala.xml.transform._

import org.json4s._

import java.io.File
import org.apache.commons.io.{FileUtils,FilenameUtils}

class BatchGeometry extends TransformerPipeline(List(new UniqueImagesRewriter(_), new UniqueEffectsRewriter(_), new BatchedGeometryRewriter(_))) {
	
	def processInputFile(outputSuffix:String, jsonSuffix:String)(path:String, content:String) = {
		val (outputXML, sideChannel) = applyPipeline(XML.loadString(content))
		val outputJSON = sideChannel.getOrElse(classOf[BatchedGeometryRewriter],JNothing)
		val outputBasePath = FilenameUtils.removeExtension(path).stripPrefix("file:")
		
		val outputDst = outputBasePath + outputSuffix
		val jsonDst = outputBasePath + jsonSuffix
		
		XML.save(outputDst, outputXML, enc="utf-8", xmlDecl = true)
		FileUtils.writeStringToFile(new File(jsonDst), native.Serialization.write(outputJSON)(native.Serialization.formats(NoTypeHints)), "utf-8")
	}
}