package visca.Scanner

import java.io.{FileReader, BufferedReader, LineNumberReader, IOException, FileNotFoundException}
import scala.compiletime.uninitialized

class SourceFile(filename: String):
  private var reader: LineNumberReader = uninitialized

  try
    reader = new LineNumberReader(new BufferedReader(new FileReader(filename)))
  catch
    case e: FileNotFoundException =>
      println(s"[# vc #]: can't read: $filename")
      sys.exit(1)

  def getNextChar: Char =
    try
      val c = reader.read()
      if c == -1 then SourceFile.eof else c.toChar
    catch
      case e: IOException =>
        println(s"Caught IOException: ${e.getMessage}")
        SourceFile.eof

  def inspectChar(nthChar: Int): Char =
    var currentNth = nthChar
    var c: Int = -1
    try
      reader.mark(nthChar)
      while currentNth != 0 do
        c = reader.read()
        currentNth -= 1
      reader.reset()
      if c == -1 then SourceFile.eof else c.toChar
    catch
      case e: IOException =>
        println(s"Caught IOException: ${e.getMessage}")
        SourceFile.eof

object SourceFile:
  val eof: Char = '\u0000'