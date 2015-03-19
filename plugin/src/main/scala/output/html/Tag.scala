package sxr
package output
package html

import Annotate.Wrapper

trait Tag { self =>
  def open: String
  def close: String
  def compose(other: Tag): Tag = new Tag { 
    def open = self.open + other.open
    def close = other.close + self.close
  }
  def andThen(other: Tag): Tag = other compose self
  def toAnnotation = Wrapper(open, close)
}

case class HtmlTag(tag: String, var attrs: Map[String, String] = Map.empty, content: String = "") extends Tag {
  
  private def attrString: String = attrs.map { case (k, v) => s"""${k}=\"${v}\""""  } mkString " "

  def apply(body: String): String = this.copy(content = body).toString
  
  def attr(name: String, value: String): this.type = {
    attrs = attrs + (name -> value);
    this
  }
  
  def addClass(klass: String): this.type = {
    val old = attrs.getOrElse("class", "").split(" ").toList
    attrs = attrs + ("class" -> ((klass :: old) mkString " "))
    this
  }
  
  def open = s"<$tag $attrString>"
  def close = s"</$tag>"
  
  override def toString: String =
    open + content + close
}