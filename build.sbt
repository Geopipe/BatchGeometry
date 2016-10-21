lazy val hereDir:File = file(".")

lazy val scalaXml = RootProject(uri(s"git:file://${hereDir.getCanonicalPath}/../../deps/scala-xml/"))

lazy val root = (project in hereDir dependsOn scalaXml).
	settings(
		name := "BatchGeometry",
		organization := "com.geopipe",
		version := "1.0",
		scalaVersion := "2.11.8"
	)
