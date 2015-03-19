package sxr

import scala.tools.nsc.doc.{ Settings => ScalaDocSettings, Index, Universe }
import scala.tools.nsc.doc.model.Entity
import language.Symbol

import java.io.File

/**
 * Additional compiler settings for scala xray
 * 
 * Those options listed below stem from the old xray implementation
 */
class Settings(error: String => Unit, printMsg: String => Unit = println(_)) 
    extends ScalaDocSettings(error, printMsg) {
  
  val sxrBaseDirectory = PathSetting (
    "-sxr-base-directory",
    "The base directory against which sources should be relativized",
    ""
  )
  
  
  val sxrExternalLink = PathSetting (
    "-sxr-external-link",
    "A file containing one URL per line for each external sxr location to link to",
    ""
  )
  
  val sxrTabSize = IntSetting(
    "-sxr-tabsize",
    "Tabs are replaced by whitespaces in the output. This setting",
    2,
    Some(0, 8),
    intParser
  )
  
  val sxrLiterateCommentStyles = ChoiceSetting(
    "-sxr-literate-comment",
    "style",
    "The selected comment style will be considered literate text",
    List("singleLine", "multiLine"),
    "singleLine"
  )
  
  def intParser(s: String): Option[Int] = try {
    Some(Integer.parseInt(s))
  } catch {
    case e: java.lang.NumberFormatException => None
  }
}

case class Context(
  baseDirectories: List[File],
  tabSize: Int,
  commentStyle: String,
  outputDirectory: File,
  universe: Universe,
  index: Index,
  models: PartialFunction[Entity, Symbol]
)