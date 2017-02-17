package com.geopipe.modeltools

import org.apache.commons.io.FileUtils
import java.io.File

object BatchGeometrySingle extends BatchGeometry with App {
	args match {
		case Array(inFile, outputSuffix, jsonSuffix) =>
			val inContent = FileUtils.readFileToString(new File(inFile))
			processInputFile(outputSuffix, jsonSuffix)(inFile, inContent)
		case _ => 
			Console.err.println("BatchGeometry takes 3 arguments: inFile, outputSuffix, jsonSuffix")
			System.exit(-1)
	}
}