import sbtassembly.AssemblyPlugin.defaultShellScript

lazy val hereDir:File = file(".")

lazy val sparkShellScript = Seq("#!/usr/bin/env sh","nthreads=$1","shift", """exec java -Dspark.master=local[$nthreads] -Dspark.app.name=BatchGeometry -jar "$0" "$@"""")

lazy val root = (project in hereDir).
	settings(
		name := "BatchGeometry",
		organization := "com.geopipe",
		version := "1.0",
		scalaVersion := "2.11.8",
		libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
		libraryDependencies += "org.apache.spark" %% "spark-core" % "2.0.1",
		mainClass in assembly := Some("com.geopipe.modeltools.BatchGeometry"),
		assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(sparkShellScript))
	)

EclipseKeys.withSource := true
