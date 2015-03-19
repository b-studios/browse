package sxr
package language

import scala.tools.nsc
import nsc.doc.model.Entity

/**
 * This trait is an interface to the compiler internal types and attempts to decouple XRay from
 * the scala compiler implementation.
 */
trait Type {
  
  /**
   * A string for the given type that should be informative, but brief.
   */
  def name: String
  
  /**
   * Pointer to the symbol defining this type
   */
  def definition: Option[Symbol]
}

/**
 * Currently this is enough. If the implementation later requires access to the compiler
 * change to a more advanced setup (see `language.Symbol`)
 */
private[sxr] case class TypeImpl(name: String, definition: Option[Symbol]) extends Type