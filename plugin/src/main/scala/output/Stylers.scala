package sxr
package output

import language.Token
import Annotate.Annotation
import java.io.File

trait Styler { self =>

  def prelude: String = ""
  def apply(token: Token): List[Annotation]
  def epilog: String = ""
  
  def compose(other: Styler): Styler = new Styler {
    override def prelude = self.prelude + other.prelude
    def apply(token: Token) = self(token) ++ other(token)
    override def epilog = other.epilog + self.epilog
  }
  
  def andThen(other: Styler): Styler = other compose self
    
  protected def annotate(f: String => String): List[Annotation] = f :: Nil
  protected def handle[A](x: A)(f: PartialFunction[A, List[Annotation]]): List[Annotation] = 
    f.applyOrElse[A, List[Annotation]](x, _ => Nil)

}