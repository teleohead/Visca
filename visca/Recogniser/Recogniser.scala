package visca.Recogniser

import visca.Scanner.{Scanner, SourceFile, SourcePosition, Token}
import visca.ErrorReporter

import scala.annotation.tailrec

sealed trait LoopStatus
case class Continue(t: Token) extends LoopStatus
case object Stop extends LoopStatus

sealed trait OptionalStatus
case class Matched(t: Token) extends OptionalStatus
case object NotPresent extends OptionalStatus

class Recogniser(private val scanner: Scanner, private val errorReporter: ErrorReporter) {
    private var currentToken: Token = scanner.getToken
    private type Result[T] = Either[SyntaxError, T]

    def parseProgram(): Unit = {
        try {
            val result: Result[Token]= fParseProgram(currentToken)
            result match {
                case Right(t) =>
                    currentToken = t
                    if (currentToken.kind != Token.EOF)
                        fail("EXTRA TOKENS", currentToken)
                case Left(_) =>
            }
        } catch {
            case _: SyntaxError =>
        }
    }

    @tailrec
    private def fKleene(t: Token)(logic: Token => Result[LoopStatus]): Result[Token]= {
        logic(t) match {
            case Right(Continue(nextT)) => fKleene(nextT)(logic)
            case Right(Stop) => Right(t)
            case Left(err) => Left(err)
        }
    }

    private def fOptional(t: Token)(logic: Token => Result[OptionalStatus]): Result[Token]= {
        logic(t) match {
            case Right(Matched(nextT)) => Right(nextT)
            case Right(NotPresent) => Right(t)
            case Left(err) => Left(err)
        }
    }

    private def fail(msg: String, t: Token): Result[Token]= {
        errorReporter.reportError(msg, t.spelling, t.position)
        Left(new SyntaxError())
    }

    private def fMatch(expected: Int, t: Token): Result[Token]= {
        if (t.kind == expected)
            Right(scanner.getToken)
        else
            fail(s"\"${Token.spell(expected)}\"", t)
    }

    private def fParseProgram(t: Token): Result[Token]= {
        if (currentToken.kind != Token.EOF) {
            for {
                t1 <- fParseDecl(t)
                y <- fParseProgram(t1)
            } yield y
        } else {
            Right(t)
        }
    }

    private def fParseDecl(t: Token): Result[Token]= {
        for {
            t1 <- fParseType(t)
            t2 <- fParseIdentifier(t1)
            y <- t2.kind match {
                case Token.LPAREN => fParseFuncDeclRemainder(t2)
                case _ => fParseVarDeclRemainder(t2)
            }
        } yield y
    }

    private def fParseVarDeclRemainder(t: Token): Result[Token]= {
        for {
            tAfterDecl <- t.kind match {
                case Token.LBRACKET =>
                    for {
                        t1 <- fMatch(Token.LBRACKET, t)
                        t2 <- t1.kind match {
                            case Token.INTLITERAL => fMatch(Token.INTLITERAL, t1)
                            case _ => Right(t1)
                        }
                        t3 <- fMatch(Token.RBRACKET, t2)
                    } yield t3
            }

            tAfterInit <- tAfterDecl.kind match {
                case Token.EQ => for {
                    t1 <- fMatch(Token.EQ, tAfterDecl)
                    t2 <- fParseInitialiser(t1)
                } yield t2
                case _ => Right(tAfterDecl)
            }

            tAfterList <- fParseInitDeclaratorList(tAfterInit)

            y <- fMatch(Token.SEMICOLON, tAfterList)
        } yield y
    }

    private def fParseFuncDeclRemainder(t: Token): Result[Token]= {
        for {
            t1 <- fParseParaList(t)
            y <- fParseCompoundStmt(t1)
        } yield y
    }

    private def fParseInitDeclaratorList(t: Token): Result[Token]= {
        for {
            tAfterInitDecl <- fParseInitDeclarator(t)
            y <- fKleene(tAfterInitDecl) { currT => currT.kind match {
                    case Token.COMMA =>
                        val result: Result[Token] = for {
                            t1 <- fMatch(Token.COMMA, currT)
                            t2 <- fParseInitDeclarator(t1)
                        } yield t2
                        result match {
                            case Right(nextT) => Right(Continue(nextT))
                            case Left(err) => Left(err)
                        }
                    case _ => Right(Stop)
                }
            }
        } yield y
    }

    private def fParseInitDeclarator(t: Token): Result[Token]= {
        for {
            tAfterDecl <- fParseDeclarator(t)

            y <- fOptional(tAfterDecl) { currT => currT.kind match {
                    case Token.EQ => for {
                            t1 <- fMatch(Token.EQ, currT)
                            t2 <- fParseInitialiser(t1)
                        } yield Matched(t2)
                    case _ => Right(NotPresent)
                }
            }
        } yield y
    }

    private def fParseDeclarator(t: Token): Result[Token]= {
        for {
            tAfterId <- fMatch(Token.ID, t)
            y <- fOptional(tAfterId) { currT => currT.kind match {
                    case Token.LBRACKET =>
                        val result: Result[Token] = for {
                            t1 <- fMatch(Token.LBRACKET, currT)
                            t2 <- t1.kind match {
                                case Token.INTLITERAL => fMatch(Token.INTLITERAL, t1)
                                case _ => Right(t1)
                            }
                            t3 <- fMatch(Token.RBRACKET, t2)
                        } yield t3
                        result.map(Matched(_))
                    case _ => Right(NotPresent)
                }
            }
        } yield y
    }

    private def fParseInitialiser(t: Token): Result[Token]= {
        t.kind match {
            case Token.LCURLY => for {
                t1 <- fMatch(Token.LCURLY, t)
                t2 <- fParseExpr(t1)
                t3 <- fKleene(t2) { currT => currT.kind match {
                        case Token.COMMA =>
                            val result = for {
                                tAfterComma <- fMatch(Token.COMMA, currT)
                                tAfterExpr <- fParseExpr(tAfterComma)
                            } yield tAfterExpr
                            result.map(Continue(_))
                        case _ => Right(Stop)
                    }
                }
                y <- fMatch(Token.RCURLY, t3)
            } yield y
            case _ => fParseExpr(t)
        }
    }

    private def fParseType(t: Token): Result[Token]= {

    }

    private def fParseIdentifier(t: Token): Result[Token]= {

    }

    private def fParseCompoundStmt(t: Token): Result[Token]= {

    }

    private def fParseStmtList(t: Token): Result[Token]= {

    }

    private def fParseStmt(t: Token): Result[Token]= {

    }

    private def fParseIfStmt(t: Token): Result[Token]= {

    }

    private def fParseForStmt(t: Token): Result[Token]= {

    }

    private def fParseWhileStmt(t: Token): Result[Token]= {

    }

    private def fParseBreakStmt(t: Token): Result[Token]= {

    }

    private def fParseBreakStmt(t: Token): Result[Token]= {

    }

    private def fParseContinueStmt(t: Token): Result[Token]= {

    }

    private def fParseReturnStmt(t: Token): Result[Token]= {

    }

    private def fParseExprStmt(t: Token): Result[Token]= {

    }

    private def fParseExpr(t: Token): Result[Token]= {

    }

    private def fParseAssignmentExpr(t: Token): Result[Token]= {

    }

    private def fParseCondOrExpr(t: Token): Result[Token]= {

    }

    private def fParseCondAndExpr(t: Token): Result[Token]= {

    }

    private def fParseEqualityExpr(t: Token): Result[Token]= {

    }

    private def fParseRelExpr(t: Token): Result[Token]= {

    }

    private def fParseAdditiveExpr(t: Token): Result[Token]= {

    }

    private def fParseUnaryExpr(t: Token): Result[Token]= {

    }

    private def fParsePrimaryExpr(t: Token): Result[Token]= {

    }

    private def fParseParaList(t: Token): Result[Token]= {

    }

    private def fParseProperParaList(t: Token): Result[Token]= {

    }

    private def fParseParaDecl(t: Token): Result[Token]= {

    }

    private def fParseArgList(t: Token): Result[Token]= {

    }

    private def fParseProperArgList(t: Token): Result[Token]= {

    }

    private def fParseArg(t: Token): Result[Token]= {

    }

    private def fParseIntLiteral(t: Token): Result[Token]= {
        if (t.kind == Token.INTLITERAL) Right(scanner.getToken)
        else fail("integer literal expected here", t)
    }

    private def fParseFloatLiteral(t: Token): Result[Token]= {
        if (t.kind == Token.FLOATLITERAL) Right(scanner.getToken)
        else fail("float literal expected here", t)
    }

    private def fParseBooleanLiteral(t: Token): Result[Token]= {
        if (t.kind == Token.BOOLEANLITERAL) Right(scanner.getToken)
        else fail("boolean literal expected here", t)
    }

    private def fParseStringLiteral(t: Token): Result[Token]= {
        fMatch(Token.STRINGLITERAL, t)
    }
}
