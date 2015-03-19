package sxr
package output
package html

import language.Symbol
import scala.tools.nsc.doc.model.MemberEntity
import utility.FileUtils.relativePath
import java.io.File

/**
 * This file encapsulates the linking scheme
 */
trait Linking {
  
  val context: Context
  
  /**
   * fromUrl has to be a source location relative to the output directory
   */
  implicit class RelativeLinkSource(fromUrl: String) {
    
    /**
     * Will throw a runtime exception if sym is not linkable
     * 
     * @param from Path to a file to link relative from
     */
    def linkTo(to: Symbol): String = (for {
      toUrl  <- to.fileUrl
    } yield { 
      val name = to.fullNameString
      val rel = if (toUrl != fromUrl) 
        relativePath(new File(fromUrl), new File(toUrl)) 
      else 
        ""
      s"$rel.html#$name"
    }).get
    
    def linkTo(member: MemberEntity): String =
        linkTo(context.models(member))
  }
  
  def isLinkable(sym: Symbol): Boolean = sym.fileUrl.isDefined
  def isLinkable(mem: MemberEntity): Boolean =
     (for {
       sym <- context.models.lift(mem)
     } yield isLinkable(sym)).getOrElse(false)
}