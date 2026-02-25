package visca

import scala.compiletime.uninitialized
import visca.Scanner.{Scanner, SourceFile, Token}

object vc {

  private var scanner: Scanner = uninitialized
  private var reporter: ErrorReporter = uninitialized
  private var currentToken: Token = uninitialized
  private var inputFilename: String = uninitialized

  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      System.err.println("Please provide the source file as an argument.")
      sys.exit(1)
    }

    inputFilename = args(0)
    println("======= The VC compiler =======")

    // Initialise the source file and error reporter
    val source = new SourceFile(inputFilename)
    reporter = new ErrorReporter()
    scanner = new Scanner(source, reporter)
    scanner.enableDebugging()

    currentToken = scanner.getToken
    while (currentToken.kind != Token.EOF) {
      currentToken = scanner.getToken
    }
  }
}