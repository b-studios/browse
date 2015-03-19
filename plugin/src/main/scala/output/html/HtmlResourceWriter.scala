package sxr
package output
package html

import java.io.File
import language.Token
import utility.FileUtils._
import scala.util.matching.Regex
import java.io.StringWriter

class HtmlResourceWriter(val context: Context) extends Writer {
  
  val extension = ".resources"
  
  import context.outputDirectory
  import HtmlResourceWriter._
    
  val cssFile = new File(outputDirectory, CSSRelativePath)
  val jsFile = new File(outputDirectory, JSRelativePath)
  val jQueryFile = new File(outputDirectory, JQueryRelativePath)

  def prepare: Unit = {
    writeDefaultCSS(cssFile)
    writeJS(jsFile)
    writeJQuery(jQueryFile)
  }
  def writeUnit(sourceFile: File, relativePath: String, tokenList: List[Token]) = {}
  def finish: Unit = {}
  
  /** Copies the default style sheet available as a resource on the classpath to the file 'to'.*/
  private def writeDefaultCSS(to: File) { writeResource(DefaultCSS, to) }
  
  /** Copies the default script available as a resource on the classpath to the file 'to'.*/
  private def writeJS(to: File) { writeResource(LinkedJS, to) }
  
  /** Copies the jQuery script available as a resource on the classpath to the file 'to'.*/
  private def writeJQuery(to: File) { writeResource(LinkedJQuery, to) }
}

object HtmlResourceWriter extends WriterFactory {
  
  def apply(context: Context) = new HtmlResourceWriter(context)
  
  /** The location to store the style sheet relative to the output directory.*/
  val CSSRelativePath = "style.css"
  
  /** The location to store the script relative to the output directory.*/
  val JSRelativePath = "linked.js"
  
  /** The location to store jQuery relative to the output directory.*/
  val JQueryRelativePath = "jquery-all.js"

  /** The path of the default style sheet resource.*/
  val DefaultCSS = "/default-style.css"
  /** The path of the default script resource.*/
  val LinkedJS = "/linked.js"
  /** The path of the JQuery resource.*/
  val LinkedJQuery = "/" + JQueryRelativePath
}