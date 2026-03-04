package visca

import scala.compiletime.uninitialized
import visca.Scanner.{Scanner, SourceFile, Token}
import visca.Recogniser.Recogniser

object vc {

  private var scanner: Scanner = uninitialized
  private var reporter: ErrorReporter = uninitialized
  private var currentToken: Token = uninitialized
  private var recogniser: Recogniser = uninitialized
  private var inputFilename: String = uninitialized

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println("Usage: java VC.vc filename\n")
      sys.exit(1)
    }

    inputFilename = args(0)
    println("======= The VC compiler =======")

    // Initialise the source file and error reporter
    val source = new SourceFile(inputFilename)
    reporter = new ErrorReporter()
    scanner = new Scanner(source, reporter)
    recogniser = new Recogniser(scanner, reporter)

    recogniser.parseProgram()
    if (reporter.getNumErrors() == 0)
        System.out.println("Compilation was successful.")
    else
        System.out.println("Compilation was unsuccessful.")
  }
}