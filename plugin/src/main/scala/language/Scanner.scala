package sxr
package language

import scala.tools.nsc
import nsc.plugins.Plugin
import scala.collection.mutable

import scala.tools.nsc.ast.parser.Tokens.{ COMMENT, USCORE, isBrace, isIdentifier, isKeyword, isLiteral }

trait Scanner { self: KnowsCompiler =>
  
  import compiler._
  
  /** 
   * Tokenizes the given source. The tokens are put into an ordered set by the start position of the token.
   * Symbols will be mapped back to these tokens by the offset of the symbol.
   */
  def scan(unit: CompilationUnit): mutable.TreeSet[Token] = {
    
    val tokens = mutable.TreeSet[Token]()
    
    def addComment(start: Int, end: Int) { tokens += new Token(start, end - start + 1, COMMENT) }

    class Scan extends syntaxAnalyzer.UnitScanner(unit) {
      override def deprecationWarning(off: Int, msg: String) {}
      override def error(off: Int, msg: String) {}
      override def incompleteInputError(off: Int, msg: String) {}

      override def foundComment(value: String, start: Int, end: Int) {
        tokens += new Token(start, end - start + 1, COMMENT) 
        super.foundComment(value, start, end)
       }
      
      // Use different token id for doc comments to filter them out later
      override def foundDocComment(value: String, start: Int, end: Int) {
        tokens += new Token(start, end - start + 1, Token.DOCCOMMENT)
        super.foundComment(value, start, end)
      }
      override def nextToken() {
        val offset0 = offset
        val code = token
        super.nextToken()

        if (includeToken(code)) {
          val length = (lastOffset - offset0) max 1
          tokens += new Token(offset0, length, code)
        }
      }
    }
    if (unit.isJava)
      new syntaxAnalyzer.JavaUnitParser(unit).parse() // TODO: Java source support
    else {
      val parser = new syntaxAnalyzer.UnitParser(unit) { override def newScanner = new Scan }
      parser.parse()
    }

    tokens
  }
  
  /** 
   * Filters out unwanted tokens such as whitespace and commas. Braces are currently
   * included because () is annotated as Unit, and a partial function created by
   * { case ... } is associated with the opening brace. 
   */
  private def includeToken(code: Int) = {
    code match {
      case COMMENT | USCORE => true
      case _ => isKeyword(code) || isIdentifier(code) || isLiteral(code) || isBrace(code)
    }
  }
}