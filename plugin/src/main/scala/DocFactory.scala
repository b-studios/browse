package sxr

import scala.tools.nsc
import nsc.interpreter._
import nsc.ast.parser.Tokens._
import nsc.doc.{ DocParser, DocFactory => ScalaDocFactory, Uncompilable, Universe }
import nsc.reporters.ConsoleReporter
import nsc.reporters.Reporter
import nsc.util.CommandLineParser
import nsc.Global

import scala.reflect.internal.util.{ NoPosition, BatchSourceFile}

import java.io._
import java.net.URLClassLoader

import nsc.doc.model
import scala.util.control.ControlThrowable

/**
 * This class is more or less a copy of `nsc.doc.DocFactory` adapted to instantiate BrowseDoclet passing on the
 * modelFactory that contains a pointer to the compiler instance.
 */
class DocFactory(reporter: Reporter, settings: Settings) { processor =>

  // The unique compiler instance used by this processor and constructed from its `settings`
  // It is this compiler instance that is used to traverse the files again and attach symbol information
  // to tokens. (Initiated in `BrowseDoclet`)
  object compiler extends Global(settings, reporter) {
    override protected def computeInternalPhases() {
      phasesSet += syntaxAnalyzer
      phasesSet += analyzer.namerFactory
      phasesSet += analyzer.packageObjects
      phasesSet += analyzer.typerFactory
    }
    override def forScaladoc = true
  }


  def document(files: List[String]) {
    import nsc.doc._
    import doclet._

    val universe = makeUniverse(Left(files)) getOrElse { throw NoCompilerRunException }

    // we pass the scaladoc compiler instance here (as member of `modelfactory.global`)...
    val docletInstance = new BrowseDoclet(settings) {
      val modelFactory: processor.modelFactory.type = processor.modelFactory
    }
    docletInstance setUniverse universe
    docletInstance setIndex model.IndexModelFactory.makeIndex(universe)
    docletInstance.generate
  }

  lazy val modelFactory = (
      new { override val global: processor.compiler.type = processor.compiler }
        with model.ModelFactory(compiler, settings)
        with model.ModelFactoryImplicitSupport
        with model.ModelFactoryTypeSupport
        with model.diagram.DiagramFactory
        with model.CommentFactory
        with model.TreeFactory
        with model.MemberLookup {
          override def templateShouldDocument(sym: compiler.Symbol, inTpl: DocTemplateImpl) =
            extraTemplatesToDocument(sym) || super.templateShouldDocument(sym, inTpl)
        }
    )

  /**
   * This is changed to allow access to the modelfactory above
   *
   * Creates a scaladoc site for all symbols defined in this call's `source`,
   * as well as those defined in `sources` of previous calls to the same processor.
   * @param source The list of paths (relative to the compiler's source path,
   *        or absolute) of files to document or the source code.
   */
  def makeUniverse(source: Either[List[String], String]): Option[Universe] = {
    assert(settings.docformat.value == "html")
    source match {
      case Left(files) =>
        new compiler.Run() compile files
      case Right(sourceCode) =>
        new compiler.Run() compileSources List(new BatchSourceFile("newSource", sourceCode))
    }

    if (reporter.hasErrors)
      return None

    modelFactory.makeModel match {
      case Some(madeModel) =>
        if (!settings.scaladocQuietRun)
          println("model contains " + modelFactory.templatesCount + " documentable templates")
        Some(madeModel)
      case None =>
        if (!settings.scaladocQuietRun)
          println("no documentable class found in compilation units")
        None
    }
  }

  private def extraTemplatesToDocument: Set[compiler.Symbol] = {
    if (settings.docUncompilable.isDefault) Set()
    else {
      val uncompilable = new {
        val global: processor.compiler.type = processor.compiler
        val settings = processor.settings
      } with Uncompilable { }

      compiler.docComments ++= uncompilable.comments

      uncompilable.templates
    }
  }

  object NoCompilerRunException extends ControlThrowable { }
}


object DocFactory {
  def apply(reporter: Reporter, settings: Settings) = (files: List[File]) =>  {
    new DocFactory(reporter, settings).document(files.map(_.toString))
  }
}
