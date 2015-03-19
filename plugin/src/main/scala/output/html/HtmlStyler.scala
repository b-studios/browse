package sxr
package output
package html

import scala.tools.nsc
import nsc.ast.parser.Tokens._
import nsc.doc.{ html => doc }
import scala.util.matching.Regex

import language.Token

import Token.DOCCOMMENT
import utility.FileUtils
import Annotate.Annotation
import scala.collection.mutable

import java.io.File
import language.Symbol

abstract class HtmlStyler(title: String)(implicit val context: Context) extends Styler with Linking {

  import scala.tools.nsc.doc.model
  import context._
  
  // Relative url to the current handled file
  val currentFile: String
  
  /**
   * All scala docs for styled tokens are collected here and later appended to the epilog
   */
  private val docs = mutable.ArrayBuffer.empty[xml.Elem]
  
  object scalaDoc extends doc.HtmlPage { 
    def title   = ""
    def headers = xml.NodeSeq.Empty
    def body    = xml.NodeSeq.Empty
    def path    = Nil // TODO initialize this one correctly
  }
  
  def apply(token: Token): List[Annotation] = handle(token) {
    case t if !t.isPlain || !tokenClass(t.code).isEmpty =>
      
      val tag = (for {
        ref <- token.reference
        if isLinkable(ref)
        url = currentFile linkTo ref
        if !isKeyword(t.code) && !isDelim(t.code)  // Do not link keywords TODO move the two methods to `Token`
      } yield HtmlTag("a")
        .attr("href", url)
        .attr("data-ref-id", ref.id) // also include ref's id for highlighting on the same page (where ids are constant)
      ).getOrElse(HtmlTag("span"))
      
      // create an id - used to associate scala doc nodes with it
      // tag.attr("id", id(token))

      tokenClass(token.code).map { tag.addClass(_) }
      token.tpe.map { tpe => tag.attr("data-type", tpe.toString) }
      //token.doc.map { doc => tag.attr("data-docs", doc.toString) }
      
      // Add FQN of symbols to definition sites
      token.definitions.map { _.fullNameString } match {
        case defs => tag.attr("data-defs", defs mkString " ")
      }
      
      // Add ids of symbols to definition sites
      token.definitions.map { _.id } match {
        case defs => tag.attr("data-def-ids", defs mkString " ")
      }
      
      renderScalaDoc(token)
        
      htmlEscape :: tag.toAnnotation :: Nil
  }
  
  override def epilog = HtmlTag("div", content = docs mkString "").addClass("scaladocs").toString
  
  private def renderScalaDoc(token: Token): Unit = token.doc match {
    case Some(m: model.DocTemplateEntity) if !isDelim(token.code) =>
      
      val templateName = m.name
      
      val scalaDoc = new doc.page.Template(universe, null, m) {
        
         // TODO add linking here
         override def relativeLinkTo(destClass: nsc.doc.model.TemplateEntity): String = {
           
           // TODO this works but this needs to be encapsulated into a linking framework.
           //   Use the same framework for linking from markdown.
           //
           //   From current file `f` link to `sym` relative to base-directories `bds`
           //   should always be the same!!!!!!!!!!
           if (context.models.isDefinedAt(destClass)) 
             context.models(destClass).fileUrl.getOrElse("#")
           else {
             println("Cannot find" + destClass)
             "#"
           }
         }
      }
      
      val body = <section data-docs-for={ /* id(token)*/ "id" }>
        <h1>{ templateName }</h1>
        
        { scalaDoc.signature(m, true) }
        
        { scalaDoc.memberToCommentBodyHtml(m, m.inTemplate, true) }

      </section>
        
      docs += body 
    case _ =>
  }
  
//  private def id(sym: Symbol) = System.identityHashCode(sym).toString
//  private def id(token: Token) = System.identityHashCode(token).toString
//  private def ids(token: Token) = token.definitions.map { id } mkString " "

  private val tokenClasses: PartialFunction[Int, String] = {
    case CHARLIT   => "char"
    case INTLIT    => "int"
    case LONGLIT   => "long"
    case FLOATLIT  => "float"
    case DOUBLELIT => "double"
    case STRINGLIT => "string"
    case SYMBOLLIT => "symbol"
    case DOCCOMMENT => "hide"
    case code if isDelim(code) => "delimiter"
    case code if isKeyword(code) => "keyword"
  }
  
  private def isDelim(code: Int): Boolean = code match {
    case LPAREN | RPAREN | LBRACKET | RBRACKET | LBRACE | RBRACE => true
    case _ => false
  }
    
  
  val escapes = new Regex("[<>&\"\t]")
  
  private def htmlEscape: Annotation = (s: String) => {
    escapes.replaceAllIn(s, _.group(0) match { 
      case ">"  => "&gt;"
      case "&"  => "&amp;"
      case "<"  => "&lt;"
      case "\"" => "&quot;"
      case "\t" => (" " * tabSize)
    })
  }
  
  private def tokenClass(code: Int): Option[String] = tokenClasses.lift(code)
}
  
  /*
     // TODO this can now be implemented as an annotating function
    // Writes the given character to the output, escaping to HTML it if necessary.
    private def write(c: Char)(implicit output: Writer): Unit = c match {
      case '>'  => output write "&gt;"
      case '&'  => output write "&amp;"
      case '<'  => output write "&lt;"
      case '"'  => output write "&quot;"
      case '\t' => output write (" " * tabSize)
      case _ => output write c
    }
   */

