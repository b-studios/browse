package sxr
package output
package html

import java.io.File
import scala.util.matching.Regex
import org.pegdown.PegDownProcessor
import org.pegdown.Extensions.{ SMARTYPANTS, ABBREVIATIONS, AUTOLINKS, TABLES, DEFINITIONS, 
                                FENCED_CODE_BLOCKS, WIKILINKS, STRIKETHROUGH }

import Annotate.Annotation
import language.Token

/**
 * A stateful styler that splits tokens into two interleaved sections `docs` and `code`
 */
abstract class CommentStyler(implicit context: Context) extends Styler {
  
  import context._
  import CommentStyle.commentStyles
  
  private object State { val OpenDocs, OpenCode = new {} }   
  import State._

  override def prelude = section.open + docs.open
  
  override def epilog = (state match {
    case OpenCode => code.close
    case OpenDocs => docs.close
  }) + section.close
  
  def apply(token: Token): List[Annotation] = handle(state) {
    // in order to preserve indentation we have to immediately open and close the code blocks and just
    // inject comments in between
    case OpenCode if token.isComment => annotate { 
      case s if isLiterate(s) => code.close + section.close + section.open + docs.open + renderComment(s) + docs.close + code.open
      case s => (renderComment andThen commentTag)(s)
    }
    // can only happen at the beginning, since we immediately close comments otherwise
    case OpenDocs if token.isComment => now(OpenCode); annotate {
      case s if !isLiterate(s) => docs.close + code.open + (renderComment andThen commentTag)(s)
      case s => renderComment(s) + docs.close + code.open
    }
    case OpenCode if !token.isComment => Nil
    case OpenDocs if !token.isComment => now(OpenCode); annotate { docs.close + code.open + _ }
  }
  
  private def renderComment = stripCommentSyntax andThen renderMarkdown
  
 
  /** Markdown and Markup **/
  private def renderMarkdown = (s: String) => (new PegDownProcessor(enabledExtensions)).markdownToHtml(s)
  private val enabledExtensions = SMARTYPANTS | ABBREVIATIONS | AUTOLINKS | TABLES | DEFINITIONS | 
                                  FENCED_CODE_BLOCKS | WIKILINKS | STRIKETHROUGH
  
  /** Comment Styles **/
  private def stripCommentSyntax = (s: String) =>
    (for (cs <- commentStyles.values.view if cs matches s)
      yield cs contentOf s).headOption.getOrElse(s)
  private def isLiterate(s: String) = commentStyles(commentStyle).matches(s)
  
  
  /** State machine **/
  private var state = State.OpenDocs
  private def now(next: AnyRef): Unit = state = next
  

  /** HTML tags **/
  private def commentTag = HtmlTag("div").addClass("comment").toAnnotation
  private def docs       = HtmlTag("div").addClass("docs")
  private def code       = HtmlTag("div").addClass("code") compose HtmlTag("code")
  private def section    = HtmlTag("section")
}

case class CommentStyle(start: String, line: String, end: String) {
    
  // TODO adapt this to the new convetion of specifying `start`
  def contentOf(comment: String): String = {
    val lines = comment.replaceFirst(start, "").lines.toList
    (lines.init :+ lines.last.replaceFirst(end, "")).map { 
      l => l.replaceFirst(line, "")
    } mkString "\n"
  }
  
  def matches(s: String): Boolean = new Regex(start).findPrefixOf(s).nonEmpty
}
object CommentStyle {
  
  val SingleLine = CommentStyle("""^\s*[/]{2}\s?""", """^\s*[/]{2}\s?""", "")
  val MultiLine = CommentStyle("""^\s*[/][*]""", """^\s*[*]\s?""", """[*]+[/]\s*$""")
  
  val commentStyles = Map(
    "singleLine" -> SingleLine,
    "multiLine"  -> MultiLine
  )
}