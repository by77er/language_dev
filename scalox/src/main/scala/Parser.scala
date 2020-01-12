package scalox

import TokenType._

class Parser(val tokens: List[Token]) {
  var current = 0

  private[this] def expression: Expr = equality

  private[this] def equality: Expr = {
    var expr = comparison

    while (matchToken(BANG_EQUAL, EQUAL_EQUAL)) {
      val operator = previous
      val right = comparison
      expr = new Binary(expr, operator, right)
    }
    expr
  }

  private[this] def matchToken(types: TokenType*): Boolean = {
    var ret = false
    types.map((t: TokenType) => {
      if (!ret && check(t)) {
        ret = true
        advance
      }
    })
    ret
  }

  private[this] def advance(): Token = {
    if (!isAtEnd) current += 1
    previous
  }

  private[this] def check(t: TokenType): Boolean = {
    if (isAtEnd) return false
    return peek.kind == t
  }

  private[this] def isAtEnd: Boolean = peek.kind == EOF

  private[this] def peek: Token = tokens(current)

  private[this] def previous: Token = tokens(current - 1)

  private[this] def comparison: Expr = {
    var expr = addition

    while(matchToken(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
      val operator = previous
      val right = addition
      expr = new Binary(expr, operator, right)
    }

    expr
  }

  private[this] def addition: Expr = {
    var expr = multiplication

    while (matchToken(MINUS, PLUS)) {
      val operator = previous
      val right = multiplication
      expr = new Binary(expr, operator, right)
    }

    expr
  }

  private[this] def multiplication: Expr = {
    var expr = unary

    while (matchToken(SLASH, STAR)) {
      val operator = previous
      val right = unary
      expr = new Binary(expr, operator, right)
    }

    expr
  }

  private[this] def unary: Expr = {
    if (matchToken(BANG, MINUS)) {
      val operator = previous
      val right = unary
      return new Unary(operator, right)
    }

    return primary
  }

  private[this] def primary: Expr = {
    if (matchToken(FALSE)) return new Literal(false.asInstanceOf[AnyRef])
    if (matchToken(TRUE))  return new Literal(true.asInstanceOf[AnyRef])
    if (matchToken(NIL))   return new Literal(null.asInstanceOf[AnyRef])

    if (matchToken(NUMBER, STRING)) {
      return new Literal(previous.literal)
    }

    if (matchToken(LEFT_PAREN)) {
      val expr = expression
      consume(RIGHT_PAREN, "Expect ')' after expression.")
      return new Grouping(expr)
    }
    throw error(peek, "Expect expression.")
  }

  private[this] def consume(kind: TokenType, message: String): Token = {
    if (check(kind)) return advance
    throw error(peek, message)
  }

  private[this] def error(token: Token, message: String): ParseError = {
    Lox.error(token, message)
    return new ParseError()
  }

  private[this] def synchronize: Unit = {
    advance
    while (!isAtEnd) {
      if (previous.kind == SEMICOLON) return;
      peek.kind match {
        case CLASS => return;
        case FUN   => return;
        case VAR   => return;
        case FOR   => return;
        case IF    => return;
        case WHILE => return;
        case PRINT => return;
        case RETURN => return;
      }

      advance
    }
  }

  def parse: Expr = {
    try {
      expression
    } catch {
      case _: ParseError => null.asInstanceOf[Expr]
    }
  }
}


class ParseError extends Exception {

}
