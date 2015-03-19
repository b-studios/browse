package sxr
package language

import scala.collection.mutable.TreeSet
import reflect.internal.util.SourceFile

import scala.tools.nsc
import nsc.plugins.Plugin
import nsc.symtab.Flags
import nsc.doc.model.Entity

import java.io.File
import scala.collection.mutable

import utility.FileUtils

trait Traversal { self: KnowsCompiler =>

  import compiler.{ Traverser, Tree, Template, TermSymbol,
     ValDef, ClassDef, ModuleDef, ValOrDefDef, TypeDef, 
     New, Return, If, Match, Alternative, Star, Bind, Ident, Throw, Literal,
     This, Select, Super, Apply, ApplyDynamic, TypeApply,
     TypeTree, TypeSymbol, PolyType, NoType, MethodType, CompoundType }
  
  import compiler.{ Symbol => CompilerSymbol, Type => CompilerType }
  
  /**
   * Dependency to the symbol factory needs to be injected in implementing classes
   */
  protected def toDocSymbol(sym: CompilerSymbol): Symbol
  protected def toDocType(name: String, sym: Option[Symbol]): Type
  
  /**
   * Dependency to the setting "baseDirectories"
   */
  val context: Context
  import context.baseDirectories
  
  class Traverse(
      tokens: TreeSet[Token], 
      source: SourceFile) extends Traverser {
 
      // magic method #1
    override def traverse(tree: Tree) = tree match {
      // tests for synthetic val created for the x in x :: Nil, which would associate the wrong type with ::
      // because the synthetic val is associated with the :: token
      case ValDef(_, _, _, rhs) if (tree.symbol != null && tree.symbol.hasFlag(Flags.SYNTHETIC)) => traverse(rhs)

      // If the first parent in the source is a trait, the first parent in parents will be AnyRef and it will
      // use the trait's token, bumping the trait.  So, this hack processes traits first
      case Template(parents, self, body) =>
        val (traits, notTraits) = parents.partition(_.symbol.isTrait)
        traverseTrees(traits)
        traverseTrees(notTraits)
        if (!self.isEmpty) traverse(self)
        traverseStats(body, tree.symbol)

      case t => process(t); super.traverse(t)
    }
    
    // magic method #2
    private def process(tree: Tree): Unit =  
      for {
        tSource <- catchToNone(tree.pos.source) if tSource == source
        token <- Token.at(tokens)(tree.pos.point)
      } tree match {
        case tt: TypeTree if token.isPlain => 
          processSymbol(tt, token, source.file.file)
      
        case _: ClassDef | _: ModuleDef | _: ValOrDefDef | _: TypeDef | _: This | _: Select |
            _: Alternative | _: Star | _: Bind | _: Ident => 
          if (tree.hasSymbol && !ignore(tree.symbol)) 
            processSymbol(tree, token, source.file.file)

        // this will annotate the 'match' keyword with the type returned by the associated pattern match
        case _: New | _: Return | _: If | _: Match | _: Throw | _: TypeApply | _: Literal => 
          token.tpe = toDocType(typeName(tree.tpe), None)

        case _: Super | _: Apply => /* Currently ignored */

        case _ => ()
      }
  
    // magic method #3
    private def processSymbol(tree: Tree, token: Token, sourceFile: File) {
      
      val sym = toDocSymbol(tree.symbol)
      
      // annotate the token with its documentation model, if available
      //   token -> model
      def addDocModel() {
        for (model <- sym.findModel) {
          token.doc = model
        }
      }
      
      // annotate the token with its source
      def addDefinition() {
        token.source = FileUtils.getRelativeSourcePath(sourceFile, baseDirectories)
        token += sym
      }

      // Annotate token with its type in field `tpe`
      def addType: PartialFunction[CompilerSymbol, Unit] = {
        
        case ts: TermSymbol =>
          Option(tree match {
            case ad: ApplyDynamic => ad.qual.tpe.memberType(ad.symbol)
            case s: Select => s.qualifier.tpe.memberType(s.symbol)
            case _ => ts.owner.thisType.memberType(ts)
          }) map {        
            case mt: MethodType if ts.hasFlag(Flags.IMPLICIT) => "implicit " + sym.fullName + " : " + typeName(mt)
            case t => typeName(t)
          } map {
            case tp => token.tpe = toDocType(tp, sym.toOption) 
          }

        case ts: TypeSymbol => 
          Option(tree.tpe match {
            case NoType => ts.info
            case sType => sType
          }) map { 
            case tp => token.tpe = toDocType(typeName(tp), toDocSymbol(tp.typeSymbol).toOption)
          }
      }

      for (s <- sym.toOption) {
        
        addDocModel()
        addType.lift(tree.symbol)
        
        if (tree.isDef) {
          addDefinition()
          
        // Currently only add references to symbols of which the file is present
        // This is important (with multiple reference assigment) to prioritize local
        // symbols over system ones.
        //
        // After adding facility to link to foreign projects this might cause problems
        } else if (tree.symbol.sourceFile != null) {
          token.reference = sym
        }
      }
    }
  }
  
  
  /** Private Utility Methods */
  
  
  /**
   * Creates a string for the given type that should be informative, but brief.
   */
  private def typeName(tpe: CompilerType): String = tpe match {
    case ct: CompoundType => compoundTypeString(ct, "") // tries to reduce size of some type strings
    case pt: PolyType if pt.typeParams.isEmpty => "=> " + typeName(pt.resultType)
    case pt: PolyType => (pt.resultType, pt.typeParams.map(_.defString).mkString("[", ", ", "]")) match {
      case (ct: CompoundType, paramsString) => compoundTypeString(ct, paramsString)
      case (_, paramsString) => paramsString + typeName(pt.resultType)
    }
    case _ if tpe == null => ""
    case _ => tpe.toString
  }
  
  /** 
   * Converts the given compound type to a string. `mainPostfix` is copied after the main type symbol
   * but before any parents or refinements
   */
  private def compoundTypeString(ct: CompoundType, mainPostfix: String) = {
    import ct._
    typeSymbol.toString + mainPostfix + ((typeSymbol.isPackageClass, parents.isEmpty, !decls.isEmpty) match {
      case (false, false, _)   => parents.mkString(" extends ", " with ", "")
      case (false, true, true) => decls.mkString("{", "; ", "}")
      case _ => ""
    })
  }
  
  /** Filters unwanted symbols, such as packages.*/
  private def ignore(s: CompilerSymbol): Boolean =
    ignoreBase(s) ||
    s.isModuleClass || // ignore the generated class for modules
    s.isPrimaryConstructor // the primary constructor overlaps with the class type, so just use the class type
  
  private def ignoreBase(s: CompilerSymbol): Boolean =
    !s.exists ||
    s.isPackage || // nothing done with packages
    s.isImplClass

  private def catchToNone[T](f: => T): Option[T] = 
    try Some(f) catch { case e: UnsupportedOperationException => None }
}  