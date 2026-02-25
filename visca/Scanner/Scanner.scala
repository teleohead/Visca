package visca.Scanner

import scala.annotation.tailrec
import visca.ErrorReporter
import visca.Scanner.{Token, SourcePosition, SourceFile}

case class State(ln: Int, col: Int, offset: Int, ch: Char)

class Scanner(sourceFile: SourceFile, errorReporter: ErrorReporter) {
  private var debug: Boolean = false
  private var currentState: State = State(1, 1, 1, sourceFile.inspectChar(1))

  def enableDebugging(): Unit = {
    debug = true
  }

  def getToken: Token = {
    val filteredState = skipSpaceAndComments(currentState)
    val (token, nextState) = nextToken(filteredState)
    currentState = nextState
    if debug then println(token)
    token
  }

  private def emit(kind: Int, spelling: String, start: State, end: State): (Token, State) = {
    val sp = SourcePosition(start.ln, end.ln, start.col, end.col)
    (Token(kind, spelling, sp), accept(end))
  }

  private def accept(s: State): State = {
    val (nextLn, nextCol) = {
      s.ch match
        case '\n' | '\r' => (s.ln + 1, 1)
        case '\t' => (s.ln, nextTabStop(s.col))
        case _ => (s.ln, s.col + 1)
    }
    val nextOffset = if s.ch == '\r' && peek(s) == '\n' then s.offset + 2 else s.offset + 1
    State(nextLn, nextCol, nextOffset, sourceFile.inspectChar(nextOffset))
  }

  @tailrec
  private def munch(s: State, acc: String, pred: Char => Boolean): (String, State) = {
    peek(s) match
      case SourceFile.eof => (acc, s)
      case nextCh if pred(nextCh) =>
        val nextState = accept(s)
        munch(nextState, acc + nextState.ch, pred)
      case _ => (acc, s)
  }

  private def nextToken(s: State): (Token, State) = {
    s.ch match
      case SourceFile.eof =>
        val sp = SourcePosition(s.ln, s.ln, 1, 1)
        (Token(Token.EOF, "$", sp), s)
      case '(' | ')' | '{' | '}' | '[' | ']' | ';' | ',' | '+' | '-' | '*' | '/' => matchPrefixFree(s)
      case '=' | '!' | '<' | '>' | '&' | '|' => matchPrefixDependent(s)
      case '"' => matchString(s)
      case '.' => peek(s) match
        case next if isVcDigit(next) => matchNumber(s)
        case _ => matchIllegal(s)
      case c if isVcDigit(c) => matchNumber(s)
      case c if isVcLetter(c) => matchIdOrBoolean(s)
      case _ => matchIllegal(s)
  }


  private def matchPrefixFree(s: State): (Token, State) = {
    s.ch match
      case '(' => emit(Token.LPAREN, "(", s, s)
      case ')' => emit(Token.RPAREN, ")", s, s)
      case '{' => emit(Token.LCURLY, "{", s, s)
      case '}' => emit(Token.RCURLY, "}", s, s)
      case '[' => emit(Token.LBRACKET, "[", s, s)
      case ']' => emit(Token.RBRACKET, "]", s, s)
      case ';' => emit(Token.SEMICOLON, ";", s, s)
      case ',' => emit(Token.COMMA, ",", s, s)
      case '+' => emit(Token.PLUS, "+", s, s)
      case '-' => emit(Token.MINUS, "-", s, s)
      case '*' => emit(Token.MULT, "*", s, s)
      case '/' => emit(Token.DIV, "/", s, s)
      case _ => emit(Token.ERROR, s.ch.toString, s, s)
  }

  private def matchPrefixDependent(s: State): (Token, State) = {
    (s.ch, peek(s)) match
      case ('=', '=') => emit(Token.EQEQ, "==", s, accept(s))
      case ('!', '=') => emit(Token.NOTEQ, "!=", s, accept(s))
      case ('<', '=') => emit(Token.LTEQ, "<=", s, accept(s))
      case ('>', '=') => emit(Token.GTEQ, ">=", s, accept(s))
      case ('&', '&') => emit(Token.ANDAND, "&&", s, accept(s))
      case ('|', '|') => emit(Token.OROR, "||", s, accept(s))
      case ('=', _) => emit(Token.EQ, "=", s, s)
      case ('!', _) => emit(Token.NOT, "!", s, s)
      case ('<', _) => emit(Token.LT, "<", s, s)
      case ('>', _) => emit(Token.GT, ">", s, s)
      case (first, _) => emit(Token.ERROR, first.toString, s, s)
  }

  private def matchString(s: State): (Token, State) = {
    def isUnterminated(c: Char) = {
      c == '\n' || c == '\r' || c == SourceFile.eof
    }

    @tailrec
    def loop(curr: State, sb: String): (String, State) = {
      val next = peek(curr)
      if isUnterminated(next) then
        val sp = SourcePosition(s.ln, s.ln, s.col, s.col)
        errorReporter.reportError("unterminated string", sb, sp)
        (sb, curr)
      else if next == '"' then
        (sb, accept(curr))
      else
        val nextState = accept(curr)
        nextState.ch match
          case '\\' =>
            val escaped = peek(nextState)
            if isUnterminated(escaped) then (sb + "\\", nextState)
            else
              val (translated, sAfterEscape) = {
                escaped match
                  case 'n' => ("\n", accept(nextState))
                  case 't' => ("\t", accept(nextState))
                  case 'b' => ("\b", accept(nextState))
                  case 'f' => ("\f", accept(nextState))
                  case 'r' => ("\r", accept(nextState))
                  case '"' => ("\"", accept(nextState))
                  case '\'' => ("\'", accept(nextState))
                  case '\\' => ("\\", accept(nextState))
                  case _ =>
                    val sp = SourcePosition(s.ln, nextState.ln, s.col, nextState.col)
                    errorReporter.reportError("illegal escape", "\\" + escaped, sp)
                    ("\\" + escaped, accept(nextState))
              }
              loop(sAfterEscape, sb + translated)
          case _ => loop(nextState, sb + nextState.ch.toString)
    }

    val (lexeme, finalState) = loop(s, "")
    emit(Token.STRINGLITERAL, lexeme, s, finalState)
  }

  private def matchNumber(s: State): (Token, State) = {
    @tailrec
    def munch(curr: State, acc: String): (String, State) = {
      peek(curr) match
        case c if isVcDigit(c) =>
          val next = accept(curr)
          munch(next, acc + next.ch)
        case _ => (acc, curr)
    }

    val (intLex: String, intState: State) = {
      if s.ch == '.' then ("", s)
      else munch(s, s.ch.toString)
    }

    val (fracLex: String, fracState: State, isFloat) = {
      peek(intState) match
        case '.' if s.ch != '.' =>
          val dotState = accept(intState)
          val (lex, state) = munch(dotState, "")
          ("." + lex, state, true)
        case _ if s.ch == '.' =>
          val (lex, state) = munch(intState, "")
          ("." + lex, state, true)
        case _ =>
          ("", intState, false)
    }

    val (expLex: String, expState: State, hasExp) = {
      peek(fracState) match
        case 'e' | 'E' =>
          val second = sourceFile.inspectChar(fracState.offset + 2)
          val third = sourceFile.inspectChar(fracState.offset + 3)
          val isValidUnsigned = isVcDigit(second)
          val isValidSigned = (second == '+' || second == '-') && isVcDigit(third)
          if isValidUnsigned || isValidSigned then
            val expSymbolState = accept(fracState)
            if isValidSigned then
              val signState = accept(expSymbolState)
              val (digits, state) = munch(signState, "")
              (expSymbolState.ch.toString + signState.ch.toString + digits, state, true)
            else
              val (digits, state) = munch(expSymbolState, "")
              (expSymbolState.ch.toString + digits, state, true)
          else
            ("", fracState, false)
        case _ => ("", fracState, false)
    }

    val finalLex = intLex + fracLex + expLex
    val kind = if isFloat || hasExp then Token.FLOATLITERAL else Token.INTLITERAL

    emit(kind, finalLex, s, expState)
  }

  private def matchIdOrBoolean(s: State): (Token, State) = {
    val (spelling, finishState) = munch(s, s.ch.toString, c => isVcLetter(c) || isVcDigit(c))
    spelling match
      case "boolean" => emit(Token.BOOLEAN, spelling, s, finishState)
      case "true" | "false" => emit(Token.BOOLEANLITERAL, spelling, s, finishState)
      case "if" => emit(Token.IF, spelling, s, finishState)
      case "else" => emit(Token.ELSE, spelling, s, finishState)
      case "int" => emit(Token.INT, spelling, s, finishState)
      case "float" => emit(Token.FLOAT, spelling, s, finishState)
      case "void" => emit(Token.VOID, spelling, s, finishState)
      case "while" => emit(Token.WHILE, spelling, s, finishState)
      case "for" => emit(Token.FOR, spelling, s, finishState)
      case "return" => emit(Token.RETURN, spelling, s, finishState)
      case "break" => emit(Token.BREAK, spelling, s, finishState)
      case "continue" => emit(Token.CONTINUE, spelling, s, finishState)
      case _ => emit(Token.ID, spelling, s, finishState)
  }

  private def matchIllegal(s: State): (Token, State) = {
    emit(Token.ERROR, s.ch.toString, s, s)
  }

  @tailrec
  private def skipSpaceAndComments(s: State): State = {
    s.ch match
      case SourceFile.eof => s
      case c if isVcWhitespace(c) => skipSpaceAndComments(accept(s))
      case '/' => peek(s) match
        case '/' => skipSpaceAndComments(skipEolComment(accept(accept(s))))
        case '*' => skipSpaceAndComments(skipTraditionalComment(s, accept(accept(s))))
        case _ => s
      case _ => s
  }

  @tailrec
  private def skipEolComment(s: State): State = {
    s.ch match
      case '\n' | '\r' => accept(s)
      case SourceFile.eof => s
      case _ => skipEolComment(accept(s))
  }

  @tailrec
  private def skipTraditionalComment(startState: State, s: State): State = {
    s.ch match
      case SourceFile.eof =>
        val sp = SourcePosition(startState.ln, startState.ln, startState.col, startState.col)
        errorReporter.reportError("unterminated comment", "", sp)
        s
      case '*' if peek(s) == '/' => accept(accept(accept(s)))
      case _ => skipTraditionalComment(startState, accept(s))
  }

  private def peek(s: State): Char = {
    sourceFile.inspectChar(s.offset + 1)
  }

  private def isVcWhitespace(c: Char): Boolean = {
    c == ' ' || c == '\t' || c == '\f' || c == '\r' || c == '\n'
  }

  private def nextTabStop(col: Int): Int = {
    col + 8 - (col - 1) % 8
  }

  private def isVcDigit(c: Char): Boolean = {
    c >= '0' && c <= '9'
  }

  private def isVcLetter(c: Char): Boolean = {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
  }
}