package sxr
package language

import scala.tools.nsc
import nsc.doc.model.Entity

import java.io.File

/**
 * This trait is an interface to the compiler internal symbols and attempts to decouple XRay from
 * the scala compiler implementation.
 */
trait Symbol {
  
  /**
   * Finds the corresponding ScalaDoc model entity
   */
  def findModel: Option[Entity]
  
  /**
   * Computes the file URL relative to the output directory
   *
   * @example {{{
   *   sym.hrefTo //=> Some("mypackage/Foo.scala")
   * }}}
   */
  def fileUrl: Option[String]
  
  /**
   * Constructs a decoded, fully qualified, normalized name for the given symbol.
   */
  def fullNameString: String
  
  /**
   * Constructs a decoded fully qualified name for the given symbol.
   */
  def fullName: String
  
  /**
   * Encapsulates the comparison with NoSymbol
   */
  def isDefined: Boolean
  
  /**
   * Allows converting symbols with underlying reference to `NoSymbol` to `Option`
   */
  def toOption: Option[Symbol] = if (isDefined) Some(this) else None
  
  /**
   * A unique id, stable for this run of the compiler
   */
  def id: String
  
  override def toString: String = fullNameString  
}

import nsc.doc.model.ModelFactory
import utility.FileUtils

private[sxr] abstract class SymbolFactory { factory =>
  
  val modelFactory: ModelFactory
  val baseDirectories: List[File]
 
  import modelFactory.global.{ Symbol => CompilerSymbol, NoSymbol }
  
  def apply(sym: CompilerSymbol): Symbol = new SymbolImpl(sym)
  
  class SymbolImpl(sym: CompilerSymbol) extends Symbol {
    
    def findModel: Option[Entity] = modelFactory.normalizeTemplate(sym) match {
      case normal if isTemplate => modelFactory.findTemplateMaybe(normal)
      case normal => findTemplate.flatMap { parent => modelFactory.findMember(normal, parent) }
    }
    
    def fileUrl: Option[String] = 
      for {
        src     <- Option(sym.sourceFile)
        srcFile <- Option(src.file)
        relSrc = FileUtils.getRelativeSourcePath(srcFile, baseDirectories)
        name   = fullNameString
      } yield relSrc

    def fullNameString: String = {
      require { sym != NoSymbol }
      val s = normalize
      val owner = s.owner
      
      require { owner != NoSymbol }
      val root = owner.isRoot || owner.isEmptyPackageClass
      val sep = if (s.isTerm) { if(root) "" else "." } else ";"
      val name = sep + s.nameString
      
      if (root) name else owner.fullNameString + name
    }
    
    def fullName: String = {
      require { sym != NoSymbol }
      val owner = sym.owner
      
      require { owner != NoSymbol }
      if (owner.isRoot || owner.isEmptyPackageClass)
        nameString
      else
        owner.enclClass.fullName + "." + nameString
    }
    
    def isDefined: Boolean = !(sym eq NoSymbol)      
      
    def id: String = System.identityHashCode(sym).toHexString
    
    private def nameString: String =
      sym.nameString + (if (isOverloadedMethod) "(" + methodHash + ")" else "")
    
    private def isOverloadedMethod: Boolean =
      sym.isMethod && sym.owner.info.member(sym.name).isOverloaded
    
    private def normalize: CompilerSymbol = sym match {
      case _ if sym.isModuleClass            => optional(sym.companionModule)       getOrElse sym
      case _ if sym.isCaseApplyOrUnapply     => optional(sym.owner.companionClass)  getOrElse sym.owner
      case _ if sym.isStable && sym.isMethod => optional(sym.getter(sym.enclClass)) getOrElse sym
      case _ => sym
    }
    
    private def optional(sym: CompilerSymbol) = if (sym eq NoSymbol) None else Some(sym)
    
    // hack: probably not sufficient to distinguish all possible overloaded methods
    // the hash can be truncated quite a bit: aren't distinguishing between many options
    private def methodHash = {
      
      import java.lang.{ Character => C }
      
      def quarter(s: String): String = if (s.length > 7) s.substring(0, s.length / 4) else s
      def hash(s: String): String = java.security.MessageDigest.getInstance("SHA").digest(s.getBytes).flatMap(hashDigits).mkString
      def hashDigits(b: Byte) = {
        val i = toInt(b)
        Array( forDigit(i >> 4), forDigit(i & 0xF) )
      }    
      def forDigit(i: Int) = C.forDigit(i, C.MAX_RADIX)
      def toInt(b: Byte): Int = if (b < 0) b + 256 else b
      
      quarter(hash(sym.tpe.toString))
    }
    
    private def findTemplate: Option[modelFactory.DocTemplateImpl] = modelFactory.normalizeTemplate(sym) match { 
      case normal if isTemplate => modelFactory.findTemplateMaybe(normal)
      case normal if normal != NoSymbol => asSymbolImpl(sym.owner).findTemplate
      case normal => 
        println("Give up")
        None
    }
  
    private def isTemplate = {
      import sym._; isClass || isTrait || isModuleOrModuleClass || isPackageObjectOrClass
    }
    
    private def asSymbolImpl(s: CompilerSymbol): SymbolImpl = factory(s).asInstanceOf[SymbolImpl]
  }
}