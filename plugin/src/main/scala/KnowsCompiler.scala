package sxr

import scala.tools.nsc.{ Global => Compiler }

trait KnowsCompiler {
  
  val compiler: Compiler

}