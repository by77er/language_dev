package scalox

object TokenType extends Enumeration {
  type TokenType = Value

  // Single-character tokens
  val LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE = Value
  val COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR = Value

  // One or two character tokens
  val BANG, BANG_EQUAL, EQUAL, EQUAL_EQUAL = Value
  val GREATER, GREATER_EQUAL, LESS, LESS_EQUAL = Value

  // Literals
  val IDENTIFIER, STRING, NUMBER = Value

  // Keywords
  val AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR = Value
  val PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE = Value

  // End of file
  val EOF = Value
}

import TokenType._

class Token(val kind: TokenType, val lexeme: String, val literal: AnyRef, val line: Int) {
  override def toString(): String = {
    s"(${kind.toString} $lexeme $literal @ $line)"
  }
}
