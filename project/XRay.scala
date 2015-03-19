import sbt._
import Keys._
import Configurations.CompilerPlugin

import sbtassembly.Plugin.{ assemblySettings => assemblyDefaults, AssemblyKeys }
import AssemblyKeys._

object XRay extends Build {
	  
	// new keys
	val js = config("js").hide
	val combineJs = TaskKey[Seq[File]]("combine-js")
	val jqueryAll = SettingKey[File]("jquery-all")
	
	  
	lazy val buildSettings = Defaults.defaultSettings ++ Seq(
		organization := "org.scala-sbt.sxr",
		version      := "0.3.1-SNAPSHOT",
		scalaVersion := "2.10.2",
		
		scalacOptions += "-deprecation"
	)
	
	lazy val assemblySettings = assemblyDefaults ++ Seq(
		jarName in assembly := "plugin_2.10-0.3.1-SNAPSHOT.jar"
	)
	
	def scalaDependencies(scalaVersion: String) = new {
	  def scalac    = "org.scala-lang" % "scala-compiler" % scalaVersion % "provided"
	  def scalarefl = "org.scala-lang" % "scala-reflect"  % scalaVersion
	  def scalalib  = "org.scala-lang" % "scala-library"  % scalaVersion
	  def pegdown   = "org.pegdown"    % "pegdown"        % "1.4.2"
	  
	  def scala = scalac :: scalarefl :: scalalib :: Nil
	  def all   = pegdown :: scala
	}
	
	def jsDependencies = {
	  
	  	val jquery_version          = "1.3.2"
			val jquery_scrollto_version = "1.4.2"
			val jquery_qtip_version     = "1.0.0-rc3"
		  val markdown_js_version     = "0.6.0-beta1"
		
			Seq(
				"jquery" % "jquery"          % jquery_version          % "js->default" 
					from ("http://jqueryjs.googlecode.com/files/jquery-" + jquery_version + ".min.js"),
				
				"jquery" % "jquery-scrollto" % jquery_scrollto_version % "js->default" 
					from ("http://flesler-plugins.googlecode.com/files/jquery.scrollTo-" + jquery_scrollto_version + "-min.js"),
				
				"jquery" % "jquery-qtip"     % jquery_qtip_version     % "js->default" 
					from ("http://craigsworks.com/projects/qtip/packages/1.0.0-rc3/jquery.qtip-" + jquery_qtip_version + ".min.js"),
					
			  "markdownjs" % "markdownjs"  % markdown_js_version     % "js->default" 
			  	from ("http://cdnjs.cloudflare.com/ajax/libs/markdown.js/" + markdown_js_version + "/markdown.min.js")
		  )
	}
	
	lazy val root = Project(
		id = "sxr",
		base = file(".")
	) aggregate(plugin, test)
	
	lazy val plugin: Project = Project(
		id = "plugin",
		base = file("plugin"),
		settings = buildSettings ++ assemblySettings ++ Seq(
			libraryDependencies <++= scalaVersion { scalaDependencies(_).all },
			libraryDependencies ++= jsDependencies,
			ivyConfigurations += js,
			exportJars := true,
			jqueryAll := target.value / "jquery-all.js",
			combineJs := combineJquery(update.value, jqueryAll.value, streams.value.log),
			resourceGenerators in Compile in plugin <+= combineJs
		)
	)
	
	lazy val test = Project(
			id = "test",
			base = file("test"),
			settings = buildSettings ++ Seq(
				scalacOptions <+= (packagedArtifact in (Compile, assembly) in plugin in packageBin) map { art =>
				  println("-Xplugin:%s" format art._2.getAbsolutePath)
					"-Xplugin:%s" format art._2.getAbsolutePath
				},
				scalacOptions ++= Seq(
					"-Xplugin-require:sxr", 
					"-P:sxr:base-directory test/src/main/scala",
					"-P:sxr:tabsize 4",
					"-P:sxr:literate-comment:multiLine"
				),
				compile <<= (compile in Compile) dependsOn (assembly in plugin)
			)
	)
	  

	private def combineJquery(report: UpdateReport, jsOut: File, log: Logger): Seq[File] = {
		IO.delete(jsOut)
		inputs(report) foreach { in => appendJs(in, jsOut) }
		log.info("Wrote combined js to " + jsOut.getAbsolutePath)
		Seq(jsOut)
	}
	private def inputs(report: UpdateReport) = report.select( configurationFilter(js.name)) sortBy { _.name }
	private def appendJs(js: File, to: File): Unit =
		Using.fileInputStream(js) { in =>
			Using.fileOutputStream(append = true)(to) { out => IO.transfer(in, out) }
		}
}