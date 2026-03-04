package visca

import visca.Scanner.SourcePosition

class ErrorReporter {
  private var numErrors: Int = 0

  /**
   * Reports an error message with the specified details.
   *
   * @param message   The error message template
   * @param tokenName The token causing the error
   * @param pos       The position of the error in the source file
   */
  def reportError(message: String, tokenName: String, pos: SourcePosition): Unit = {
    System.out.printf("ERROR: %d(%d)..%d(%d): ",
      pos.lineStart, pos.charStart, pos.lineFinish, pos.charFinish)

    if (message.indexOf('%') < 0) {
      print(message)
//      if (!tokenName.isEmpty) print(": " + tokenName)
    } else {
      var i = 0
      while (i < message.length) {
        val ch = message.charAt(i)
        if (ch == '%') print(tokenName) else print(ch)
        i += 1
      }
    }

    println()
    numErrors += 1
  }

  /**
   * Reports a restriction message.
   *
   * @param message The restriction message
   */
  def reportRestriction(message: String): Unit = {
    println("RESTRICTION: " + message)
  }

  /**
   * Gets the total number of errors reported.
   *
   * @return The number of errors
   */
  def getNumErrors(): Int = numErrors
}