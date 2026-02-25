package visca.Scanner

class SourcePosition(var lineStart: Int, var lineFinish: Int, var charStart: Int, var charFinish: Int) {
  def this() = {
    this(0, 0, 0, 0)
  }

  def this(lineStart: Int, lineFinish: Int) = {
    this(lineStart, lineFinish, 0, 0)
  }

  override def toString: String = {
    s"$lineStart($charStart)..$lineFinish($charFinish)"
  }
}