package visca.Recogniser

import visca.Scanner.{Scanner, SourceFile, SourcePosition, Token}
import visca.ErrorReporter

class Recogniser(private val scanner: Scanner, private val errorReporter: ErrorReporter) {
    private var currentToken: Token = scanner.getToken
    type Result = Either[SyntaxError, Token]

    def parseProgram(): Unit = {
        try {
            val result: Result = fParseProgram(currentToken)
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

    private def fail(msg: String, t: Token): Result = {
        errorReporter.reportError(msg, t.spelling, t.position)
        Left(new SyntaxError())
    }

    private def fMatch(expected: Int, t: Token): Result = {
        if (t.kind == expected)
            Right(scanner.getToken)
        else
            fail(s"\"${Token.spell(expected)}\"", t)
    }

    private def fParseProgram(t: Token): Result = {

    }

    private def fParseDecl(t: Token): Result = {

    }

    private def fParseVarDeclRemainder(t: Token): Result = {

    }

    private def fParseFuncDeclRemainder(t: Token): Result = {

    }

    private def fParseInitDeclaratorList(t: Token): Result = {

    }

    private def fParseInitDeclarator(t: Token): Result = {

    }

    private def fParseDeclarator(t: Token): Result = {

    }

    private def fParseInitialiser(t: Token): Result = {

    }

    private def fParseType(t: Token): Result = {

    }

    private def fParseIdentifier(t: Token): Result = {

    }

    private def fParseCompoundStmt(t: Token): Result = {

    }

    private def fParseStmtList(t: Token): Result = {

    }

    private def fParseStmt(t: Token): Result = {

    }

    private def fParseIfStmt(t: Token): Result = {

    }

    private def fParseForStmt(t: Token): Result = {

    }

    private def fParseWhileStmt(t: Token): Result = {

    }

    private def fParseBreakStmt(t: Token): Result = {

    }

    private def fParseBreakStmt(t: Token): Result = {

    }

    private def fParseContinueStmt(t: Token): Result = {

    }

    private def fParseReturnStmt(t: Token): Result = {

    }

    private def fParseExprStmt(t: Token): Result = {

    }

    private def fParseExpr(t: Token): Result = {

    }

    private def fParseAssignmentExpr(t: Token): Result = {

    }

    private def fParseCondOrExpr(t: Token): Result = {

    }

    private def fParseCondAndExpr(t: Token): Result = {

    }

    private def fParseEqualityExpr(t: Token): Result = {

    }

    private def fParseRelExpr(t: Token): Result = {

    }

    private def fParseAdditiveExpr(t: Token): Result = {

    }

    private def fParseUnaryExpr(t: Token): Result = {

    }

    private def fParsePrimaryExpr(t: Token): Result = {

    }

    private def fParseParaList(t: Token): Result = {

    }

    private def fParseProperParaList(t: Token): Result = {

    }

    private def fParseParaDecl(t: Token): Result = {

    }

    private def fParseArgList(t: Token): Result = {

    }

    private def fParseProperArgList(t: Token): Result = {

    }

    private def fParseArg(t: Token): Result = {

    }

    private def fParseIntLiteral(t: Token): Result = {
        if (t.kind == Token.INTLITERAL) Right(scanner.getToken)
        else fail("integer literal expected here", t)
    }

    private def fParseFloatLiteral(t: Token): Result = {
        if (t.kind == Token.FLOATLITERAL) Right(scanner.getToken)
        else fail("float literal expected here", t)
    }

    private def fParseBooleanLiteral(t: Token): Result = {
        if (t.kind == Token.BOOLEANLITERAL) Right(scanner.getToken)
        else fail("boolean literal expected here", t)
    }

    private def fParseStringLiteral(t: Token): Result = {
        fMatch(Token.STRINGLITERAL, t)
    }
}
