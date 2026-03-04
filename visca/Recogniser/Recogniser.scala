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
            fParseProgram(currentToken) match {
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
    private def fKleene(t: Token)(logic: Token => Result[LoopStatus]): Result[Token] = {
        logic(t) match {
            case Right(Continue(nextT)) => fKleene(nextT)(logic)
            case Right(Stop) => Right(t)
            case Left(err) => Left(err)
        }
    }

    private def fOptional(t: Token)(logic: Token => Result[OptionalStatus]): Result[Token] = {
        logic(t).map {
            case Matched(next) => next
            case NotPresent => t
        }
    }

    private def fail[T](msg: String, t: Token): Result[T] = {
        errorReporter.reportError(msg, t.spelling, t.position)
        Left(new SyntaxError(msg))
    }

    private def fMatch(expected: Int, t: Token): Result[Token] = {
        if (t.kind == expected)
            Right(scanner.getToken)
        else
            fail(s"\"${Token.spell(expected)}\" expected here", t)
    }

    private def fParseProgram(t: Token): Result[Token] = {
        fKleene(t) { curr =>
            curr.kind match {
                case Token.EOF => Right(Stop)
                case Token.VOID | Token.INT | Token.FLOAT | Token.BOOLEAN => fParseDecl(curr).map(Continue(_))
                case _ => fail(s"\"${curr.spelling}\" wrong result type for a function", curr)
            }
        }
    }

    private def fParseDecl(t: Token): Result[Token] = {
        for {
            t1 <- fParseType(t)
            t2 <- fParseIdentifier(t1)
            y <- t2.kind match {
                case Token.LPAREN => fParseFuncDeclRemainder(t2)
                case _ => fParseVarDeclRemainder(t2)
            }
        } yield y
    }

    private def fParseVarDeclRemainder(t: Token): Result[Token] = {
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
                case _ => Right(t)
            }

            tAfterInit <- tAfterDecl.kind match {
                case Token.EQ => for {
                    t1 <- fMatch(Token.EQ, tAfterDecl)
                    t2 <- fParseInitialiser(t1)
                } yield t2
                case _ => Right(tAfterDecl)
            }

            tAfterList <- fKleene(tAfterInit) { curr =>
                curr.kind match {
                    case Token.COMMA => for {
                        t1 <- fMatch(Token.COMMA, curr)
                        t2 <- fParseInitDeclarator(t1)
                    } yield Continue(t2)
                    case _ => Right(Stop)
                }
            }

            y <- fMatch(Token.SEMICOLON, tAfterList)
        } yield y
    }

    private def fParseFuncDeclRemainder(t: Token): Result[Token] = {
        for {
            t1 <- fParseParaList(t)
            y <- fParseCompoundStmt(t1)
        } yield y
    }

    private def fParseInitDeclarator(t: Token): Result[Token] = {
        for {
            tAfterDecl <- fParseDeclarator(t)

            y <- fOptional(tAfterDecl) { curr =>
                curr.kind match {
                    case Token.EQ => for {
                            t1 <- fMatch(Token.EQ, curr)
                            t2 <- fParseInitialiser(t1)
                    } yield Matched(t2)
                    case _ => Right(NotPresent)
                }
            }
        } yield y
    }

    private def fParseDeclarator(t: Token): Result[Token] = {
        for {
            tAfterId <- fMatch(Token.ID, t)
            y <- fOptional(tAfterId) { curr =>
                curr.kind match {
                    case Token.LBRACKET => for {
                            t1 <- fMatch(Token.LBRACKET, curr)
                            t2 <- t1.kind match {
                                case Token.INTLITERAL => fMatch(Token.INTLITERAL, t1)
                                case _ => Right(t1)
                            }
                            t3 <- fMatch(Token.RBRACKET, t2)
                        } yield Matched(t3)
                    case _ => Right(NotPresent)
                }
            }
        } yield y
    }

    private def fParseInitialiser(t: Token): Result[Token] = {
        t.kind match {
            case Token.LCURLY => for {
                t1 <- fMatch(Token.LCURLY, t)
                t2 <- fParseExpr(t1)
                t3 <- fKleene(t2) { curr =>
                    curr.kind match {
                        case Token.COMMA => for {
                                tAfterComma <- fMatch(Token.COMMA, curr)
                                tAfterExpr <- fParseExpr(tAfterComma)
                            } yield Continue(tAfterExpr)
                        case _ => Right(Stop)
                    }
                }
                y <- fMatch(Token.RCURLY, t3)
            } yield y
            case _ => fParseExpr(t)
        }
    }

    private def fParseType(t: Token): Result[Token] = {
        t.kind match {
            case Token.VOID | Token.INT | Token.FLOAT | Token.BOOLEAN => fMatch(t.kind, t)
            case _ => fail("type expected", t)
        }
    }

    private def fParseIdentifier(t: Token): Result[Token] = {
        t.kind match {
            case Token.ID => fMatch(Token.ID, t)
            case _ => fail("identifier expected", t)
        }
    }

    private def fParseCompoundStmt(t: Token): Result[Token] = {
        for {
            t1 <- fMatch(Token.LCURLY, t)
            t2 <- fKleene(t1) { curr =>
                curr.kind match {
                    case Token.VOID | Token.INT | Token.FLOAT | Token.BOOLEAN =>
                        fParseDecl(curr).map(Continue(_))
                    case _ => Right(Stop)
                }
            }
            t3 <- fKleene(t2) { curr =>
                curr.kind match {
                    case Token.RCURLY => Right(Stop)
                    case _ => fParseStmt(curr).map(Continue(_))
                }
            }
            y <- fMatch(Token.RCURLY, t3)
        } yield y
    }

    private def fParseStmt(t: Token): Result[Token] = {
        t.kind match {
            case Token.LCURLY => fParseCompoundStmt(t)
            case Token.IF => fParseIfStmt(t)
            case Token.FOR => fParseForStmt(t)
            case Token.WHILE => fParseWhileStmt(t)
            case Token.RETURN => fParseReturnStmt(t)
            case Token.BREAK => fParseBreakStmt(t)
            case Token.CONTINUE => fParseContinueStmt(t)
            case _ => fParseExprStmt(t)
        }
    }

    private def fParseIfStmt(t: Token): Result[Token] = {
        for {
            t1 <- fMatch(Token.IF, t)
            t2 <- fMatch(Token.LPAREN, t1)
            t3 <- fParseExpr(t2)
            t4 <- fMatch(Token.RPAREN, t3)
            t5 <- fParseStmt(t4)
            y <- fOptional(t5) { curr =>
                curr.kind match {
                    case Token.ELSE => for {
                        y1 <- fMatch(Token.ELSE, curr)
                        y2 <- fParseStmt(y1)
                    } yield Matched(y2)
                    case _ => Right(NotPresent)
                }
            }
        } yield y
    }

    private def fParseForStmt(t: Token): Result[Token] = {
        for {
            t1 <- fMatch(Token.FOR, t)
            t2 <- fMatch(Token.LPAREN, t1)
            t3 <- fOptional(t2)(parseExprIfTypeIsNot(Token.SEMICOLON))
            t4 <- fMatch(Token.SEMICOLON, t3)
            t5 <- fOptional(t4)(parseExprIfTypeIsNot(Token.SEMICOLON))
            t6 <- fMatch(Token.SEMICOLON, t5)
            t7 <- fOptional(t6)(parseExprIfTypeIsNot(Token.RPAREN))
            t8 <- fMatch(Token.RPAREN, t7)
            y <- fParseStmt(t8)
        } yield y
    }

    private def fParseWhileStmt(t: Token): Result[Token] = {
        for {
            t1 <- fMatch(Token.WHILE, t)
            t2 <- fMatch(Token.LPAREN, t1)
            t3 <- fParseExpr(t2)
            t4 <- fMatch(Token.RPAREN, t3)
            y <- fParseStmt(t4)
        } yield y
    }

    private def fParseBreakStmt(t: Token): Result[Token] = {
        for {
            t1 <- fMatch(Token.BREAK, t)
            y <- fMatch(Token.SEMICOLON, t1)
        } yield y
    }

    private def fParseContinueStmt(t: Token): Result[Token] = {
        for {
            t1 <- fMatch(Token.CONTINUE, t)
            y <- fMatch(Token.SEMICOLON, t1)
        } yield y
    }

    private def fParseReturnStmt(t: Token): Result[Token] = {
        for {
            t1 <- fMatch(Token.RETURN, t)
            t2 <- fOptional(t1)(parseExprIfTypeIsNot(Token.SEMICOLON))
            y <- fMatch(Token.SEMICOLON, t2)
        } yield y
    }

    private def fParseExprStmt(t: Token): Result[Token] = {
        for {
            tAfterExpr <- fOptional(t)(parseExprIfTypeIsNot(Token.SEMICOLON))
            y <- fMatch(Token.SEMICOLON, tAfterExpr)
        } yield y
    }

    private def fParseExpr(t: Token): Result[Token] = {
        fParseAssignmentExpr(t)
    }

    private def fParseAssignmentExpr(t: Token): Result[Token] = {
        for {
            t1 <- fParseCondOrExpr(t)
            y <- fOptional(t1) { curr =>
                curr.kind match {
                    case Token.EQ => for {
                        t11 <- fMatch(Token.EQ, curr)
                        t12 <- fParseAssignmentExpr(t11)
                    } yield Matched(t12)
                    case _ => Right(NotPresent)
                }
            }
        } yield y
    }

    private def fParseCondOrExpr(t: Token): Result[Token] = {
        for {
            t1 <- fParseCondAndExpr(t)
            y <- fKleene(t1) { curr =>
                curr.kind match {
                    case Token.OROR => for {
                        t11 <- fMatch(Token.OROR, curr)
                        t12 <- fParseCondAndExpr(t11)
                    } yield Continue(t12)
                    case _ => Right(Stop)
                }
            }
        } yield y

    }

    private def fParseCondAndExpr(t: Token): Result[Token] = {
        for {
            t1 <- fParseEqualityExpr(t)
            y <- fKleene(t1) { curr =>
                curr.kind match {
                    case Token.ANDAND => for {
                        t11 <- fMatch(Token.ANDAND, curr)
                        t12 <- fParseEqualityExpr(t11)
                    } yield Continue(t12)
                    case _ => Right(Stop)
                }
            }
        } yield y
    }

    private def fParseEqualityExpr(t: Token): Result[Token] = {
        for {
            t1 <- fParseRelExpr(t)
            y <- fKleene(t1) { curr =>
                curr.kind match {
                    case Token.EQEQ | Token.NOTEQ => for {
                        t11 <- fMatch(curr.kind, curr)
                        t12 <- fParseRelExpr(t11)
                    } yield Continue(t12)
                    case _ => Right(Stop)
                }
            }
        } yield y
    }

    private def fParseRelExpr(t: Token): Result[Token] = {
        for {
            t1 <- fParseAdditiveExpr(t)
            y <- fKleene(t1) { curr =>
                curr.kind match {
                    case Token.LT | Token.LTEQ | Token.GT | Token.GTEQ => for {
                        t11 <- fMatch(curr.kind, curr)
                        t12 <- fParseAdditiveExpr(t11)
                    } yield Continue(t12)
                    case _ => Right(Stop)
                }
            }
        } yield y
    }

    private def fParseAdditiveExpr(t: Token): Result[Token] = {
        for {
            t1 <- fParseMultiplicativeExpr(t)
            y <- fKleene(t1) { curr =>
                curr.kind match {
                    case Token.PLUS | Token.MINUS => for {
                        t11 <- fMatch(curr.kind, curr)
                        t12 <- fParseMultiplicativeExpr(t11)
                    } yield Continue(t12)
                    case _ => Right(Stop)
                }
            }
        } yield y
    }

    private def fParseMultiplicativeExpr(t: Token): Result[Token] = {
        for {
            t1 <- fParseUnaryExpr(t)
            y <- fKleene(t1) { curr =>
                curr.kind match {
                    case Token.MULT | Token.DIV => for {
                        t11 <- fMatch(curr.kind, curr)
                        t12 <- fParseUnaryExpr(t11)
                    } yield Continue(t12)
                    case _ => Right(Stop)
                }
            }
        } yield y
    }

    private def fParseUnaryExpr(t: Token): Result[Token] = {
        t.kind match {
            case Token.PLUS | Token.MINUS | Token.NOT =>
                for {
                    t1 <- fMatch(t.kind, t)
                    t2 <- fParseUnaryExpr(t1)
                } yield t2
            case _ => fParsePrimaryExpr(t)
        }
    }

    private def fParsePrimaryExpr(t: Token): Result[Token] = {
        t.kind match {
            case Token.ID => fParseIdentifier(t).flatMap { next =>
                next.kind match {
                    case Token.LPAREN => fParseArgList(next)
                    case Token.LBRACKET => for {
                        t1 <- fMatch(Token.LBRACKET, next)
                        t2 <- fParseExpr(t1)
                        t3 <- fMatch(Token.RBRACKET, t2)
                    } yield t3
                    case _ => Right(next)
                }
            }

            case Token.LPAREN => for {
                t1 <- fMatch(Token.LPAREN, t)
                t2 <- fParseExpr(t1)
                t3 <- fMatch(Token.RPAREN, t2)
            } yield t3

            case Token.INTLITERAL | Token.FLOATLITERAL |
                 Token.BOOLEANLITERAL | Token.STRINGLITERAL => fMatch(t.kind, t)

            case _ => fMatch(Token.SEMICOLON, t)
        }
    }

    private def fParseParaList(t: Token): Result[Token] = {
        for {
            t1 <- fMatch(Token.LPAREN, t)
            t2 <- t1.kind match {
                case Token.RPAREN => Right(t1)
                case _ => fParseProperParaList(t1)
            }
            y <- fMatch(Token.RPAREN, t2)
        } yield y
    }

    private def fParseProperParaList(t: Token): Result[Token] = {
        for {
            t1 <- fParseParaDecl(t)
            y <- fKleene(t1) { curr =>
                curr.kind match {
                    case Token.COMMA => for {
                        t11 <- fMatch(Token.COMMA, curr)
                        t12 <- fParseParaDecl(t11)
                    } yield Continue(t12)
                    case _ => Right(Stop)
                }
            }
        } yield y
    }

    private def fParseParaDecl(t: Token): Result[Token] = {
        for {
            t1 <- fParseType(t)
            y <- fParseDeclarator(t1)
        } yield y
    }

    private def fParseArgList(t: Token): Result[Token] = {
        for {
            t1 <- fMatch(Token.LPAREN, t)
            t2 <- t1.kind match {
                case Token.RPAREN => Right(t1)
                case _ => fParseProperArgList(t1)
            }
            y <- fMatch(Token.RPAREN, t2)
        } yield y
    }

    private def fParseProperArgList(t: Token): Result[Token] = {
        for {
            t1 <- fParseArg(t)
            y <- fKleene(t1) { curr =>
                curr.kind match {
                    case Token.COMMA => for {
                        t11 <- fMatch(Token.COMMA, curr)
                        t12 <- fParseArg(t11)
                    } yield Continue(t12)
                    case _ => Right(Stop)
                }
            }
        } yield y
    }

    private def fParseArg(t: Token): Result[Token] = {
        fParseExpr(t)
    }

    private def fParseIntLiteral(t: Token): Result[Token] = {
        if (t.kind == Token.INTLITERAL) Right(scanner.getToken)
        else fail("integer literal expected here", t)
    }

    private def fParseFloatLiteral(t: Token): Result[Token] = {
        if (t.kind == Token.FLOATLITERAL) Right(scanner.getToken)
        else fail("float literal expected here", t)
    }

    private def fParseBooleanLiteral(t: Token): Result[Token] = {
        if (t.kind == Token.BOOLEANLITERAL) Right(scanner.getToken)
        else fail("boolean literal expected here", t)
    }

    private def fParseStringLiteral(t: Token): Result[Token] = {
        fMatch(Token.STRINGLITERAL, t)
    }

    private val parseExprIfTypeIsNot: Int => Token => Result[OptionalStatus] = tokenType => t => {
        t.kind match {
            case `tokenType` => Right(NotPresent)
            case _ => fParseExpr(t).map(Matched(_))
        }
    }
}
