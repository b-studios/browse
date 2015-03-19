package sxr
package output

import utility.FileUtils.{ withReader, withWriter, withStringWriter }
import language.Token

import java.io.{File, Reader, Writer => JWriter}

/** 
 * The entry point for annotating an input file.
 */
object Annotate {
  
  /**
   * An annotation is just a function that may wrap or annotate the passed string as necessary
   */
  type Annotation = String => String
  
  /** 
   * Annotates an input source file with highlighting and type information provided by 'tokens' 
   * and applied by 'styler'.
   * The result is written to 'target'.
   */
  def apply(source: File, sourceEncoding: String, target: File, tokens: List[Token], styler: Styler) {
    withReader(source, sourceEncoding) { input =>
      withWriter(target) { output =>
        new Annotate(input, output, tokens, styler).annotate()
      }
    }
  }
  
  case class Wrapper(open: String, close: String) extends Annotation {
    def apply(s: String) = Seq(open, s, close) mkString ""
  }  
}

/** 
 * Annotates a source file.
 * This class is one-time use (because of the input/output Reader/Writer) and should only be used 
 * through the Annotate module.
 *
 *  'input' is the raw source file.
 * The annotated file will be written to 'output'
 * The information associated with a file is defined by 'tokens'
 * 'styler' generates the final annotations for a token 
 *
 * Note that the 'write' method in this class is specific to HTML and would
 * need to be generalized for another format
 */
class Annotate(input: Reader, output: JWriter, tokens: List[Token], styler: Styler) {
  
  private implicit val out = output
  
  /** Applies the annotations.*/
  def annotate() {
    output.write(styler.prelude)
    annotate(0, tokens)
    output.write(styler.epilog)
  }

  /** Applies annotations.  index is the current position in the source file. */
  private def annotate(index: Int, tokens: List[Token]): Unit = tokens match {
    
    // no more tokens, copy the remaining characters over
    case Nil => transfer(java.lang.Integer.MAX_VALUE)
    
    //look at the next token
    case token :: tail if token.start < index =>
      println("Overlapping span detected at index " + index + ": " + token)
      annotate(index, tail)
    
    case token :: tail =>
      // copy over characters not to be annotated
      transfer(token.start - index)
      
      // get the string to be annotated
      val annotateMe = readToString(token.length)
      
      // apply all annotations in order
      val styled = (styler(token) :+ identity[String] _) reduce (_ andThen _) apply annotateMe
      output.write(styled)
      
      // continue
      annotate(token.start + token.length, tail)
  }

  private def readToString(chars: Int): String =
    withStringWriter { writer => transfer(chars)(writer) }
  
  /** Transfers the given number of characters from the input to the output unless the input does not have enough
   * characters, in which case all remaining characters are transferred.*/
  private def transfer(chars: Int)(implicit output: JWriter): Unit = {
    if (chars > 0) {
      val c = input.read()
      if(c >= 0) {
        output.write(c.asInstanceOf[Char])
        transfer(chars - 1)(output)
      }
    }
  }
}