package sxr

import scala.tools.nsc
import nsc.doc.doclet.{ Generator, Indexer, Universer }
import nsc.doc.model.ModelFactory
import utility.FileUtils
import language.{ Scanner, Traversal, SymbolFactory, Symbol, Token, TypeImpl }

import output.Writer
import output.html.{ HtmlWriter, HtmlIndexWriter, HtmlResourceWriter }

import java.io.File

/**
 * This class represents the connection between scala doc and xray
 *
 * In order to access the scaladoc model from XRay, tokens contain a member `doc` that points to the
 * corresponding scala doc.
 *
 * In the other direction a hashmap provides the mapping between models and symbols.
 */
abstract class BrowseDoclet(val settings: Settings)
    extends Generator with KnowsCompiler with Indexer with Universer with Scanner with Traversal { doclet =>

  val modelFactory: ModelFactory
  final lazy val compiler: modelFactory.global.type = modelFactory.global
  import compiler.{ currentRun, Symbol => CompilerSymbol }

  def generateImpl(): Unit = {

    val units = for (unit <- currentRun.units.toList; sourceFile <- FileUtils.getSourceFile(compiler)(unit)) yield {
      val tokens = scan(unit)

      val traverser = new Traverse(tokens, unit.source)
      traverser(unit.body)

      (sourceFile, tokens.toList)
    }

    writers.foreach { _.prepare }
    for ((sourceFile, tokens) <- units; writer <- writers) {
      writer.writeUnit(sourceFile, FileUtils.getRelativeSourcePath(sourceFile, context.baseDirectories), tokens)
    }
    writers.foreach { _.finish }
  }

  /**
   * Register Writers - currently this is done by hand.
   */
  private lazy val writers = Seq(HtmlWriter, HtmlResourceWriter, HtmlIndexWriter).map { _.apply(context) }


  /**
   * Injecting Dependencies
   */
  private lazy val symFactory = new SymbolFactory { factory =>
    final val modelFactory: doclet.modelFactory.type  = doclet.modelFactory
    final val baseDirectories = doclet.baseDirectories
  }
  protected def toDocSymbol(sym: CompilerSymbol) = symFactory(sym)
  protected def toDocType(name: String, sym: Option[Symbol]) = TypeImpl(name, sym)


  /**
   * Setting up config values and other context information
   */
  lazy val context = Context(
    tabSize = settings.sxrTabSize.value,
    commentStyle = settings.sxrLiterateCommentStyles.value,
    outputDirectory = {
      val dir = new File(classDirectory.getParentFile, classDirectory.getName + ".sxr")
      dir.mkdirs
      dir
    },
    baseDirectories = baseDirectories,
    universe = universe,
    index = index,
    models = {
      case e: modelFactory.type#EntityImpl => toDocSymbol(e.sym)
    }
  )

  private lazy val baseDirectories = settings.sxrBaseDirectory.value.split(File.pathSeparator).map(new File(_)).toList

  private lazy val classDirectory = {
    val single = settings.outputDirs.getSingleOutput
    val orDefault = single.flatMap(f => Option(f.file)) getOrElse new File(".")
    orDefault.getAbsoluteFile
  }
}
