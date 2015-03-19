package sxr
package output

import java.io.File

import scala.tools.nsc
import nsc.doc.Universe

import sxr.language.Token

trait Writer {
    
  val extension: String
  val context: Context
    
  def prepare: Unit
  def writeUnit(sourceFile: File, relativePath: String, tokenList: List[Token])
  def finish: Unit
    
  def getOutputFile(relativePath: String) =
    new File(context.outputDirectory, relativePath + extension)
}
trait WriterFactory extends (Context => Writer)