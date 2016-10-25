import sbtassembly.AssemblyPlugin.defaultShellScript

lazy val hereDir:File = file(".")

lazy val root = (project in hereDir).
	settings(
		name := "BatchGeometry",
		organization := "com.geopipe",
		version := "1.0",
		scalaVersion := "2.11.8",
		libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
		mainClass in assembly := Some("com.geopipe.modeltools.BatchGeometry"),
		assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultShellScript))
	)

EclipseKeys.withSource := true
