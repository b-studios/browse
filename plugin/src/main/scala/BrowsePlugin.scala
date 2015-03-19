package sxr

import scala.tools.nsc
import nsc.{ Global, Phase, CompilerCommand }
import nsc.plugins.{ Plugin, PluginComponent }

import utility.FileUtils

import java.io.File

/**
 * This class only contains members that are necessary to be run as a compiler plugin.
 * The actual work is done by `DocFactory` and then `BrowseDoclet`.
 *
 * A birds eye view at the implementation of this plugin renders as follows
 * 
 *                                                                       ┌──────────────┐
 *     ┏━━━━━━━━━━━━━━┓ instantiates and ┌────────────┐ instantiates and │ BrowseDoclet │ 
 *     ┃ BrowsePlugin ┃─────────────────→│ DocFactory │─────────────────→├──────────────┤
 *     ┗━━━━━━━━━━━━━━┛   delegates to   └──┬─────────┘   delegates to   │ modelFactory │
 *                                          │ new                        │   universe   │
 *                                        ╭─┴──────╮                     │    index     │
 *                                        │ scalac │                     └──────────────┘
 *                                        ╰────────╯
 *
 * 
 * Here DocFactory creates a custom scalac compiler instance and then runs scaladoc to create a `modelFactory`
 * a documentation `universe` and an `index`. All three are passed as arguments to `BrowseDoclet`.
 * 
 * In particular the compiler instance itself is passed as member of `modelFactory.global`.
 */
class BrowsePlugin(val global: Global) extends Plugin { plugin =>
  
  // The original compiler instance is just used to gather information about the settings, list of files to
  // process etc. All following steps ignore this compiler instance and only refer to a new custom one.
  import global.{ currentRun, reporter }
  
  val name = "sxr"
  val description = "A plugin to produce a browsable representation of the input sources."
  val components = List[PluginComponent](Component)

  def run: Unit = {
    // get the list of source files from the original compiler command invocation
    val sourceFiles: List[File] = currentRun.units.toList.flatMap(FileUtils.getSourceFile(global)(_));

    // then call the doclet
    DocFactory(reporter, settings)(sourceFiles)
  }
  
  private object Component extends PluginComponent {
    val global = plugin.global
    val phaseName = plugin.name
    def newPhase(prev: Phase) = new BrowsePhase(prev)
    val runsAfter = "typer" :: Nil
    override val runsBefore = "patmat" :: Nil
  }

  private class BrowsePhase(prev: Phase) extends Phase(prev) {
    def name = plugin.name
    def run = plugin.run
  }
  
  // Handle Settings
  lazy val settings = new Settings(println) { self =>
    embeddedDefaults[BrowseDoclet]
    classpath.value = getClasspath
    
    // copy selected settings
    global.settings.outputDirs.getSingleOutput.map { self.outputDirs.setSingleOutput(_) }
  }
  
  override def processOptions(options: List[String], error: String => Unit) {
    // prefix the options with -sxr- to allow parsing with the settings parser
    options.map { "-sxr-" + _ }.foreach(settings.processArgumentString)
    println("OPTIONS " + settings)
  }
  
  // @see https://www.assembla.com/code/scala-eclipse-toolchain/git/nodes/test/scaladoc/scala/html/HtmlFactoryTest.scala?rev=22bd4c04188b2581f694970f372bd322395a3184
  private def getClasspath = {
    
    def classLoader = Thread.currentThread.getContextClassLoader

    def urls(cl: ClassLoader): Array[String] =
      cl.asInstanceOf[java.net.URLClassLoader].getURLs.map(_.getPath)
    
    val paths = urls(classLoader)
    val morepaths = urls(classLoader.getParent)
    
    (paths ++ morepaths).mkString(File.pathSeparator)
  }
}