/**
 * sxr -- Scala X-Ray
 * Copyright 2009 Mark Harrah
 */
package sxr
package language

import scala.tools.nsc.doc.model
import scala.tools.nsc.ast.parser.{ Tokens => tokens }
import scala.collection.SortedSet

/** 
 *  Represents a token at the lexer level with associated type information.
 * 'start' is the offset of the token in the original source file.
 * 'length' is the length of the token in the original source file
 * 'code' is the class of the token (see Tokens in the compiler)
 */
case class Token(start: Int, length: Int, code: Int) extends Ordered[Token] with Comparable[Token] {
  
  require(start >= 0, "Start index of a token needs to be >= 0")
  require(length > 0, "Length of the range covered by a token needs to be greater then 0")
  
  /**
   * Tokens are sorted by their start position, so that they may be searched by offset in
   * the extraction code and then processed in sequence in the annotation code. 
   */
  def compare(other: Token): Int = start compare other.start
  
  private[this] var rawType: Option[Type] = None
  private[this] var rawReferences: List[Symbol] = Nil
  private[this] var rawDefinitions: Set[Symbol] = Set.empty
  private[this] var rawSource: Option[String] = None
  private[this] var scalaDoc: Option[model.Entity] = None

  /** Type information for this token. */
  def tpe: Option[Type] = rawType
  def tpe_=(t: Type): Unit = if (rawType.isEmpty) rawType = Some(t)

  /** Link to the defining location for this token. 
   *  
   *  TODO as it turns out this is not always the right choice. In the text-file `debug_references` this becomes
   *  visible.
   *  
   *  For instance for
   *  
   *     List(scala.Symbol, scala.Symbol.apply, wae.package.sym2exp)
   *  
   *  and
   *  
   *      List(defs.Defs.v_=.x$1, defs.Defs.v, defs.Defs.v)
   *  
   *  the last entry is the desired one. Maybe follow the same strategy as with definitions and just
   *  include all of them in the output 
   */
  def reference: Option[Symbol] = rawReferences.headOption
  def reference_=(l: Symbol): Unit = { rawReferences ::= l } //if (rawReference.isEmpty) rawReference = Some(l)

  /** The relative path of the source containing this token. */
  def source: Option[String] = rawSource
  def source_=(s: String): Unit = if (rawSource.isEmpty) rawSource = Some(s)

  /** ScalaDoc model for this token */
  def doc: Option[model.Entity] = scalaDoc
  def doc_=(m: model.Entity): Unit = if (scalaDoc.isEmpty) scalaDoc = Some(m)
  
  /** Collects the symbols pointing to this token */
  def definitions: Set[Symbol] = rawDefinitions
  def +=(s: Symbol) { rawDefinitions += s }

  /** True if this token has no reference to a definition and has no definitions itself. */
  def isSimple: Boolean = reference.isEmpty && definitions.isEmpty

  /** True if this token has no reference to a definition, has no definitions itself, and
  * has no type information. */
  def isPlain: Boolean = isSimple && tpe.isEmpty

  def isComment: Boolean = code == tokens.COMMENT
  
  override def toString: String = 
    "Token@" + Token.nameOf(code) + "(" + start + ", " + (start + length) + ")"
}

object Token {
  import scala.annotation.switch

  final val DOCCOMMENT = 120
  
  @switch def nameOf(tokenType: Int): String = tokenType match {
    case  -3 => "EMPTY"
    case  -2 => "UNDEF"
    case  -1 => "ERROR"
    case   0 => "EOF"
    case   1 => "CHARLIT"
    case   2 => "INTLIT"
    case   3 => "LONGLIT"
    case   4 => "FLOATLIT"
    case   5 => "DOUBLELIT"
    case   6 => "STRINGLIT"
    case   7 => "STRINGPART"
    case   8 => "SYMBOLLIT"
    case   9 => "INTERPOLATIONID"
    case  10 => "IDENTIFIER"
    case  11 => "BACKQUOTED_IDENT"
    case  20 => "IF"
    case  21 => "FOR"
    case  22 => "ELSE"
    case  23 => "THIS"
    case  24 => "NULL"
    case  25 => "NEW"
    case  26 => "WITH"
    case  27 => "SUPER"
    case  28 => "CASE"
    case  29 => "CASECLASS"
    case  30 => "CASEOBJECT"
    case  31 => "VAL"
    case  32 => "ABSTRACT"
    case  33 => "FINAL"
    case  34 => "PRIVATE"
    case  35 => "PROTECTED"
    case  36 => "OVERRIDE"
    case  37 => "IMPLICIT"
    case  38 => "VAR"
    case  39 => "DEF"
    case  40 => "TYPE"
    case  41 => "EXTENDS"
    case  42 => "TRUE"
    case  43 => "FALSE"
    case  44 => "OBJECT"
    case  45 => "CLASS"
    case  46 => "IMPORT"
    case  47 => "PACKAGE"
    case  48 => "YIELD"
    case  49 => "DO"
    case  50 => "TRAIT"
    case  51 => "SEALED"
    case  52 => "THROW"
    case  53 => "TRY"
    case  54 => "CATCH"
    case  55 => "FINALLY"
    case  56 => "WHILE"
    case  57 => "RETURN"
    case  58 => "MATCH"
    case  59 => "FORSOME"
    case  61 => "LAZY"
    case  62 => "MACRO"
    case  63 => "THEN"
    case  70 => "COMMA"
    case  71 => "SEMI"
    case  72 => "DOT"
    case  73 => "USCORE"
    case  74 => "COLON"
    case  75 => "EQUALS"
    case  76 => "LARROW"
    case  77 => "ARROW"
    case  78 => "NEWLINE"
    case  79 => "NEWLINES"
    case  80 => "SUBTYPE"
    case  81 => "SUPERTYPE"
    case  82 => "HASH"
    case  83 => "AT"
    case  84 => "VIEWBOUND"
    case  90 => "LPAREN"
    case  91 => "RPAREN"
    case  92 => "LBRACKET"
    case  93 => "RBRACKET"
    case  94 => "LBRACE"
    case  95 => "RBRACE"
    case  96 => "XMLSTART"
    case  97 => "COMMENT"
    case 105 => "WHITESPACE"
    case 106 => "IGNORE"
    case 109 => "ESCAPE"
    case 120 => "DOCCOMMENT"
  }
  
  /** 
   * Gets the token for the given offset.
   */
  def at(tokens: SortedSet[Token])(offset: Int): Option[Token] =
    tokensAt(tokens)(offset).headOption
    
  /** 
   * Gets the token for the given offset.
   * Create artificial tokens to get a subset of the tokens starting at the given offset
   * then, take the first token in the range
   */
  private def tokensAt(tokens: SortedSet[Token])(offset: Int): List[Token] =
    tokens.range(new Token(offset, 1, 0), new Token(offset+1, 1, 0)).toList
}
