package sxr
package output
package html

import java.io.File
import scala.util.matching.Regex
import java.util.regex.Matcher

import language.Token
import utility.FileUtils
import FileUtils._

class HtmlIndexWriter(val context: Context) extends Writer with Linking {

  import context._

  val extension = ".js"

  def prepare: Unit = {

    val out = getOutputFile("index")

    withWriter(out) { o => o.write(renderIndex(index)) }
  }

  def writeUnit(sourceFile: File, relativePath: String, tokenList: List[Token]) = {}

  def finish: Unit = {}

  /** Rendering of the index
   *  TODO maybe this should be done on the client side using a json file rendered only once... */
  import scala.tools.nsc.doc.Index
  import scala.tools.nsc.doc.model.MemberEntity
  def renderIndex(index: Index): String = {

    def renderEntry(member: MemberEntity) = {
      import scala.tools.nsc.doc
      import doc.html.page.Template
      import xml.NodeSeq

      import doc.base._
      import doc.base.comment._

      import doc.model._

      import scala.xml.{ NodeSeq, Text, UnprefixedAttribute }

      val scalaDocRenderer = new Template(context.universe, null, member.inTemplate) {

        // strangely the rendered links are still empty...
        override def templateToPath(tpl: TemplateEntity): List[String] = "foo" :: "Path" :: Nil

        // We don't want html, so return a node seq that represents plain text
        override def inlineToHtml(inline: comment.Inline) = NodeSeq fromSeq Seq(xml.Text(inlineToStr(inline)))

        // Copied since private
        private def inlineToStr(inl: comment.Inline): String = inl match {
          case comment.Chain(items) => items flatMap (inlineToStr(_)) mkString ""
          case comment.Italic(in) => inlineToStr(in)
          case comment.Bold(in) => inlineToStr(in)
          case comment.Underline(in) => inlineToStr(in)
          case comment.Monospace(in) => inlineToStr(in)
          case comment.Text(text) => text
          case comment.Summary(in) => inlineToStr(in)
          case _ => inl.toString
        }

        def nameClassStr(mbr: MemberEntity): String =
          if (mbr.isImplicitlyInherited)
            if (mbr.isShadowedOrAmbiguousImplicit)
              "implicit shadowed"
            else
              "implicit"
          else
            "name"

        def nameStr(mbr: MemberEntity): String =
          if (mbr.isConstructor) mbr.inTemplate.name else mbr.name

        def tparamsToStr(mbr: Any): String = mbr match {
          case hk: HigherKinded =>
            val tpss = hk.typeParams
            if (tpss.isEmpty) "" else {
              def tparam0(tp: TypeParam): String =
                tp.variance + tp.name + tparamsToStr(tp) + boundsToHtml(tp.hi, tp.lo, false).toString
              def tparams0(tpss: List[TypeParam]): String = (tpss: @unchecked) match {
                case tp :: Nil => tparam0(tp)
                case tp :: tps => tparam0(tp) + ", " + tparams0(tps)
              }
             "[" + tparams0(tpss) + "]"
            }
          case _ => ""
        }
        def paramsToStr(vlsss: List[List[ValueParam]]): String = {
          def param0(vl: ValueParam): String =
            vl.name + ": " + typeToHtml(vl.resultType, false).toString

          def implicitCheck(vlss: List[ValueParam]): String = vlss match {
            case vl :: vls if(vl.isImplicit) => "implicit "
            case _ => ""
          }
          vlsss.map { vlss => "(" + implicitCheck(vlss) + (vlss.map(param0) mkString ", ") + ")"} mkString ""
        }

        def printSignature(mbr: MemberEntity): String = Seq(
          mbr.flags.map(f => inlineToStr(f.text)).mkString(" "),
          kindToString(mbr),
          " ",
          nameStr(mbr),
          tparamsToStr(mbr),
          mbr match {
            case cls: Class => paramsToStr(cls.valueParams)
            case ctr: Constructor => paramsToStr(ctr.valueParams)
            case dfe: Def => paramsToStr(dfe.valueParams)
            case _ => ""
          },
          (mbr match {
            case tme: MemberEntity if (tme.isDef || tme.isVal || tme.isLazyVal || tme.isVar) =>
              ": " + typeToHtml(tme.resultType, false)
            case abt: MemberEntity with AbstractType => boundsToHtml(abt.hi, abt.lo, false)
            case alt: MemberEntity with AliasType => typeToHtml(alt.alias, false)
            case tpl: MemberTemplateEntity if !tpl.parentTypes.isEmpty =>
              s" extends " + typeToHtml(tpl.parentTypes.map(_._2), false)
            case _ => ""
          }).toString
        ) mkString ""
      }

      // the signature still contains links that should be removed
      // TODO add current path
      s"""{
        |  name: "${ member.name }",
        |  fqn: "${ member.definitionName }",
        |  signature: "${ scalaDocRenderer.printSignature(member) }",
        |  url: "${ "." linkTo member }"
        |}""".stripMargin
    }

    val members = index.firstLetterIndex.values.flatMap { _.values }.flatten.toList
        .sortBy { _.name }
        .filter { isLinkable }

    s"""var index = [
       |${members.map(renderEntry) mkString ",\n"}
       |];""".stripMargin
  }

  // escapes linebreaks and quotes
  def escapeToJsonString(s: String) =
      s.replaceAll("\"", """\\"""").replaceAll("\n","""\\n""")
}
object HtmlIndexWriter extends WriterFactory {

  def apply(context: Context) = new HtmlIndexWriter(context)

}
