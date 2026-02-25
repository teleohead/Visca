package visca.Scanner

import scala.compiletime.uninitialized
import visca.Scanner.SourcePosition

final class Token {
  var kind: Int = uninitialized
  var spelling: String = uninitialized
  var position: SourcePosition = uninitialized

  def this(kind: Int, spelling: String, position: SourcePosition) = {
    this()
    if (kind == Token.ID) {
      this.kind = determineKeywordKind(spelling)
    } else {
      this.kind = kind
    }
    this.spelling = spelling
    this.position = position
  }

  private def determineKeywordKind(spelling: String): Int = {
    var i = Token.firstReservedWord
    while i <= Token.lastReservedWord do
      if Token.keywords(i) == spelling then return i
      i += 1
    Token.ID
  }

  override def toString: String = {
    s"Kind = $kind [${Token.spell(kind)}], spelling = \"$spelling\", position = $position"
  }

}

object Token {
  def spell(kind: Int): String = keywords(kind)

  // reserved words - must be in alphabetical order...
  val BOOLEAN: Int        = 0
  val BREAK: Int          = 1
  val CONTINUE: Int       = 2
  val ELSE: Int           = 3
  val FLOAT: Int          = 4
  val FOR: Int            = 5
  val IF: Int             = 6
  val INT: Int            = 7
  val RETURN: Int         = 8
  val VOID: Int           = 9
  val WHILE: Int          = 10

  // operators
  val PLUS: Int           = 11
  val MINUS: Int          = 12
  val MULT: Int           = 13
  val DIV: Int            = 14
  val NOT: Int            = 15
  val NOTEQ: Int          = 16
  val EQ: Int             = 17
  val EQEQ: Int           = 18
  val LT: Int             = 19
  val LTEQ: Int           = 20
  val GT: Int             = 21
  val GTEQ: Int           = 22
  val ANDAND: Int         = 23
  val OROR: Int           = 24

  // separators
  val LCURLY: Int         = 25
  val RCURLY: Int         = 26
  val LPAREN: Int         = 27
  val RPAREN: Int         = 28
  val LBRACKET: Int       = 29
  val RBRACKET: Int       = 30
  val SEMICOLON: Int      = 31
  val COMMA: Int          = 32

  // identifiers
  val ID: Int             = 33

  // literals
  val INTLITERAL: Int     = 34
  val FLOATLITERAL: Int   = 35
  val BOOLEANLITERAL: Int = 36
  val STRINGLITERAL: Int  = 37

  // special tokens...
  val ERROR: Int          = 38
  val EOF: Int            = 39

  private val keywords: Array[String] = Array(
    "boolean", "break", "continue", "else", "float", "for", "if", "int", "return", "void", "while",
    "+", "-", "*", "/", "!", "!=", "=", "==", "<", "<=", ">", ">=", "&&", "||",
    "{", "}", "(", ")", "[", "]", ";", ",",
    "<id>", "<int-literal>", "<float-literal>", "<boolean-literal>", "<string-literal>", "<error>", "$"
  )

  private val firstReservedWord: Int = Token.BOOLEAN
  private val lastReservedWord: Int = Token.WHILE
}