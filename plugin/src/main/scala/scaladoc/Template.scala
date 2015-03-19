package scaladoc

import scala.tools.nsc.doc
import doc.html.page
import doc.Universe
import doc.model.{ MemberEntity, TemplateEntity }

class Template(universe: Universe, member: MemberEntity) extends page.Template(universe, null, member.inTemplate) {

  override def templateToPath(tpl: TemplateEntity): List[String] = "Path" :: Nil
  
}