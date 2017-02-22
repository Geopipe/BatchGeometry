package com.geopipe.modeltools

import org.apache.commons.io.FileUtils
import java.io.File

import com.geopipe.profiling.TicToc.{tic,toc}

object BatchGeometrySingle extends BatchGeometry with App {
	args match {
		case Array(inFile, outputSuffix, jsonSuffix) =>
			tic
			val inContent = FileUtils.readFileToString(new File(inFile))
			toc("readFileToString")
			processInputFile(outputSuffix, jsonSuffix)(inFile, inContent)
		case _ => 
			Console.err.println("BatchGeometry takes 3 arguments: inFile, outputSuffix, jsonSuffix")
			System.exit(-1)
	}
}