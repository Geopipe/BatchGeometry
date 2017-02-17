package com.geopipe.modeltools

import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

object BatchGeometrySpark extends BatchGeometry with App {	
	args match {
		case Array(dirPattern, outputSuffix, jsonSuffix) =>
			SparkContext.getOrCreate.wholeTextFiles(dirPattern).foreach{
				case (path, content) =>
					processInputFile(outputSuffix, jsonSuffix) _
			}
		case _ => 
			Console.err.println("BatchGeometry takes 3 arguments: dirPattern, outputSuffix, jsonSuffix")
			System.exit(-1)
	}
}