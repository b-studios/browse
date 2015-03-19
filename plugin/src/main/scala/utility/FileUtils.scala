/* sxr -- Scala X-Ray
 * Copyright 2009 Mark Harrah
 */
package sxr
package utility

import scala.tools.nsc.Global
import java.io.{ FileInputStream, FileOutputStream, InputStream, OutputStream, StringWriter }
import java.io.{ BufferedReader, BufferedWriter, File, FileReader, FileWriter,
  InputStreamReader, OutputStreamWriter }
import java.nio.charset.CharacterCodingException
import java.net.URL


/**
 *  A collection of utilities for I/O
 */
object FileUtils {

  val DefaultEncoding = "UTF-8"

  val encodings = "UTF-8" :: "ISO-8859-1" :: "UTF-16" :: Nil

  /** Managed resource operation.*/
  def withReader[T](source: File, sourceEncoding: String = DefaultEncoding)(f: BufferedReader => T): T = {
    val in = tryAllEncodings(source, sourceEncoding)
    try f(in) finally in.close();
  }

  /** Managed resource operation.*/
  def withWriter[T](target: File)(f: BufferedWriter => T): T = {
    target.getParentFile.mkdirs()
    val output = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(target), DefaultEncoding))
    try f(output) finally output.close()
  }

  def withStringWriter[T](f: StringWriter => T): String = {
    val sw = new StringWriter
    try { f(sw); sw.getBuffer.toString } finally sw.close()
  }

  /** Get the number of common path components from the root.*/
  private def commonPrefix[S](a: Array[S], b: Array[S]): Int = {
    def common(count: Int): Int = {
      if (count >= a.length || count >= b.length || a(count) != b(count))
        count
      else
        common(count+1)
    }
    common(0)
  }



  private def tryAllEncodings(source: File, sourceEncoding: String): BufferedReader =
    (for {
      enc <- (sourceEncoding :: encodings).view
      input <- tryToOpen(source, enc)
    } yield input).headOption match {
      case None => sys.error(s"Cannot open $source due to issues with the encoding")
      case Some(in) => in
    }

  private def tryToOpen(source: File, sourceEncoding: String): Option[BufferedReader] =
    try {
      Some(new BufferedReader(new InputStreamReader(new FileInputStream(source), sourceEncoding)))
    } catch {
      case e: CharacterCodingException => None
    }


  /** Converts the given file to an array of path component strings. */
  private def toPathArray(file: File): Array[String] = {
    def toPathList(f: File, current: List[String]): List[String] = {
      if(f == null)
        current
      else
        toPathList(f.getParentFile, f.getName :: current)
    }
    toPathList(file.getAbsoluteFile, Nil).toArray
  }

  /** Creates a relative path from 'fromFile' to 'toFile' (for use in an 'href' attribute).*/
  def relativePath(fromFile: File, toFile: File): String = {
    val fromPath = toPathArray(fromFile)
    val toPath = toPathArray(toFile)
    val commonLength = commonPrefix(fromPath, toPath)
    val relativeTo = toPath.drop(commonLength)
    val parentsToCommon = (fromPath.length - commonLength - 1)
    require(parentsToCommon >= 0)
    val up = "../" * parentsToCommon
    relativeTo.mkString(up, "/", "")
  }

  def readChars(source: File, sourceEncoding: String = DefaultEncoding): Array[Char] = {
    scala.io.Source.fromFile(source).toArray
  }

  /** Copies the 'resource' to be found on the classpath to the file 'to'.*/
  def readResourceAsString(resource: String): String = {
    val source = getClass.getResourceAsStream(resource)
    if (source == null)
      sys.error("Could not find resource " + resource)

    val out = new StringBuilder();

    try scala.io.Source.fromInputStream(source).getLines().mkString("\n") finally source.close()
  }

  /** Copies the 'resource' to be found on the classpath to the file 'to'.*/
  def writeResource(resource: String, to: File) {
    val source = getClass.getResourceAsStream(resource)
    if (source == null)
      sys.error("Could not find resource " + resource)
    try write(source, to) finally source.close()
  }

  /** Writes the 'input' stream to the file 'to'.*/
  private def write(input: InputStream, to: File) {
    to.getParentFile.mkdirs()
    val out = new FileOutputStream(to)
    try transfer(input, out) finally out.close()
  }

  /** Copies all bytes from the 'input' stream to the 'output' strem. */
  private def transfer(input: InputStream, out: OutputStream) {
    val buffer = new Array[Byte](8192)
    def transfer() {
      val read = input.read(buffer)
      if (read >= 0) {
        out.write(buffer, 0, read)
        transfer()
      }
    }
    transfer()
  }

  def getRelativeSourcePath(source: File, baseDirectories: List[File]): String = baseDirectories.flatMap {
    base => FileUtils.relativize(base, source)
   } match {
    case Nil => source.getName
    case x :: Nil => x
    case xs => xs reduceLeft shortest
  }

  private def shortest(a: String, b: String) = if (a.length < b.length) a else b

  /** Relativies the path of the given file against the given base file.*/
  def relativize(baseFile: File, file: File): Option[String] = {
    val pathString = file.getAbsolutePath
    baseFileString(baseFile) flatMap { baseString =>
      if (pathString.startsWith(baseString))
        Some(pathString.substring(baseString.length))
      else
        None
    }
  }

  private[sxr] def getSourceFile(compiler: Global)(unit: compiler.CompilationUnit): Option[File] = unit.source.file.file match {
    case null => None // code compiled from the repl has no source file
    case f: File => Some(f.getAbsoluteFile)
  }

  // used by relativize
  private def baseFileString(baseFile: File): Option[String] = {
    if (baseFile.isDirectory) {
      val cp = baseFile.getAbsolutePath
      assert(cp.length > 0)
      if (cp.charAt(cp.length - 1) == File.separatorChar)
        Some(cp)
      else
        Some(cp + File.separatorChar)
    }
    else
      None
  }

  def readLines[T](file: File, encoding: String, value: T)(f: (T, String) => T): T =
    withReader(file, encoding) { reader => readLines(reader, value)(f) }

  private final def readLines[T](reader: BufferedReader, value: T)(f: (T, String) => T): T = {
    val line = reader.readLine()
    if (line eq null) value else readLines(reader, f(value, line))(f)
  }

  def readLines(file: File, encoding: String)(f: (String) => Unit): Unit =
    withReader(file, encoding) { reader => readLines(reader)(f) }

  private final def readLines(reader: BufferedReader)(f: (String) => Unit): Unit = {
    val line = reader.readLine()
    if (line ne null) { f(line); readLines(reader)(f) }
  }

  def download(url: URL, file: File) {
    download(url.openStream, file)
  }

  def downloadCompressed(url: URL, file: File) {
    download(new java.util.zip.GZIPInputStream(url.openStream), file)
  }

  def download(in: InputStream, file: File) {
    try write(in, file) finally in.close()
  }
}
