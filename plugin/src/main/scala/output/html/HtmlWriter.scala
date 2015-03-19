package sxr
package output
package html

import java.io.File
import scala.util.matching.Regex
import java.util.regex.Matcher

import language.Token
import utility.FileUtils
import FileUtils._

class HtmlWriter(implicit val context: Context) extends Writer with Linking {

  import context._

  val extension = ".html"

  def prepare: Unit = {}

  def writeUnit(sourceFile: File, relativePath: String, tokenList: List[Token]) = {

    val out = getOutputFile(relativePath)

    // not using compactTokens at the moment, since it leads to
    // char encoding issues.
    val compactTokens = compactComments(sourceFile, tokenList)

    val title = "Test Title"

    val stylers =
        new CommentStyler { val currentFile = relativePath } andThen
        new HtmlStyler(title) { val currentFile = relativePath }


    // type info should be stored in data-type
    val content = withStringWriter { out =>
      withReader(sourceFile, "UTF-8") { in =>
        new Annotate(in, out, compactTokens, stylers).annotate()
      }
    }

    withWriter(out) { o =>
      o.write(render(tpl)(Map(
          "title" -> title,
          "content" -> content,
          "index" -> renderIndex(context.index)
      )))
    }
  }

  def finish: Unit = {}

  /** Rendering of the index
   *  TODO maybe this should be done on the client side using a json file rendered only once... */
  import scala.tools.nsc.doc.Index
  import scala.tools.nsc.doc.model.MemberEntity
  def renderIndex(index: Index): String = {

    def renderLetterGroup(letter: Char, nameGroups: Iterable[xml.Elem]) =
      <dl class="letter">
        <dt>{ letter }</dt>
        { nameGroups }
      </dl>

    def renderNameGroup(name: String, entries: Iterable[xml.Elem]) =
      <dl class="name">
        <dt>{ name }</dt>
        <ul>
          { entries }
        </ul>
      </dl>

    // TODO adapt linkTo
    def renderEntry(member: MemberEntity) = {
      val scalaDocRenderer = new scaladoc.Template(context.universe, member)
      val modifier =  scalaDocRenderer.visibility(member).map {
        case vis => <span class="visibility">{ vis }</span>
      }.getOrElse("")

      <li>
        <h4>{ member.name }</h4>
        { modifier }
        <span class="signature">{ scalaDocRenderer.signature(member, true) }</span>
        <span class="path">{ "." linkTo member }</span>
      </li>
    }

    val members = index.firstLetterIndex.values.flatMap { _.values }.flatten.toList
        .sortBy { _.name }
        .filter { isLinkable }

    <ul class="index">
      { members map renderEntry }
    </ul>.toString
  }


  /** Templating */

  val Template = "/template.html"

  lazy val tpl = readResourceAsString(Template);

  def render(template: String)(vars: Map[String, String]) =
    varsRegex.replaceAllIn(template, m => { Matcher.quoteReplacement(vars(m.group(1))) })

  lazy val varsRegex = new Regex("""\{\{([^}]+)\}\}""")


  /**
   * Collapse many single-line comments into one token.
   */
  private def compactComments(sourceFile: File, tokens: List[Token]): List[Token] = {

    import scala.tools.nsc.ast.parser.Tokens._

    var aggr: Option[Token] = None
    val compactTokens = scala.collection.mutable.ArrayBuffer.empty[Token]

    val source: Array[Char] = readChars(sourceFile)

    for (token <- tokens)
      (token.isComment && isLineComment(token), aggr) match {
        case (true, None) => aggr = Some(token)

        // The only thing allowed between comments is whitespace
        case (true, Some(com)) if isWhitespace(sourceBetween(com, token)) =>
          aggr = Some(new Token(com.start, token.start + token.length - com.start, COMMENT))

        case _ => reset(token)
      }

    // finally push aggregated comment, if it is still left
    aggr.map { compactTokens.append(_) }

    def reset(token: Token) {
      aggr.map { compactTokens.append(_) }
      aggr = None
      compactTokens.append(token)
    }

    def tokenSource(t: Token): String =
      source.slice(t.start, t.start + t.length) mkString ""

    def sourceBetween(start: Token, end: Token): String =
      source.slice(start.start + start.length, end.start) mkString ""

    def isLineComment(t: Token): Boolean =
      tokenSource(t).matches("""^\/\/.*""")

    def isWhitespace(s: String): Boolean =
      s.matches ("""^\s+$""")

    compactTokens.toList
  }
}
object HtmlWriter extends WriterFactory {
  def apply(context: Context) = new HtmlWriter()(context)
}
