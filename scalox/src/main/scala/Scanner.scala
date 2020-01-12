package scalox

import collection.immutable.HashMap
import TokenType._

class Scanner(val source: String) {
  // I may come back and fix this very non-scala mess later
  var tokens: List[Token] = List()
  var start: Int = 0
  var current: Int = 0
  var line: Int = 1
  val keywords = HashMap(
    "and"    -> AND,
    "class"  -> CLASS,
    "else"   -> ELSE,
    "false"  -> FALSE,
    "for"    -> FOR,
    "fun"    -> FUN,
    "if"     -> IF,
    "nil"    -> NIL,
    "or"     -> OR,
    "print"  -> PRINT,
    "return" -> RETURN,
    "super"  -> SUPER,
    "this"   -> THIS,
    "true"   -> TRUE,
    "var"    -> VAR,
    "while"  -> WHILE
  )

  def scanTokens(): List[Token] = {
    while (!isAtEnd()) {
      // We're at the beginning of the next lexeme
      start = current
      scanToken
    }

    tokens = new Token(EOF, "", null, line) :: tokens
    return tokens
  }

  private[this] def isAtEnd(): Boolean = current >= source.length

  private[this] def scanToken(): Unit = {
    val c: Char = advance

    // first check: single-character lexemes
    c match {
      case '(' => addToken(LEFT_PAREN)
      case ')' => addToken(RIGHT_PAREN)
      case '{' => addToken(LEFT_BRACE)
      case '}' => addToken(RIGHT_BRACE)
      case ',' => addToken(COMMA)
      case '.' => addToken(DOT)
      case '-' => addToken(MINUS)
      case '+' => addToken(PLUS)
      case ';' => addToken(SEMICOLON)
      case '*' => addToken(STAR)
      case '!' => addToken(if (matchChar('=')) BANG_EQUAL else BANG)
      case '=' => addToken(if (matchChar('=')) EQUAL_EQUAL else EQUAL)
      case '<' => addToken(if (matchChar('=')) LESS_EQUAL else LESS)
      case '>' => addToken(if (matchChar('=')) GREATER_EQUAL else GREATER)
      // Distinguish division from comments
      case '/' => if (matchChar('/')) {
        while (peek != '\n' && !isAtEnd) advance
      } else {
        addToken(SLASH)
      }
      // Ignore whitespace
      case ' '  =>
      case '\r' =>
      case '\t' =>
      case '\n' => line += 1
      // Handle literals
      case '"' => string
      case i if i.isDigit => number
      case a if isAlpha(a) => identifier
      case _   => Lox.error(line, "Unexpected character.")
    }
  }

  private[this] def advance: Char = {
    current += 1
    source.charAt(current - 1)
  }

  private[this] def addToken(kind: TokenType, literal: AnyRef = null): Unit = {
    val text: String = source.substring(start, current)
    tokens = new Token(kind, text, literal, line) :: tokens
  }

  private[this] def isAlpha(c: Char): Boolean = {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
  }

  private[this] def isAlphaNumeric(c: Char): Boolean = {
    isAlpha(c) || c.isDigit
  }

  private[this] def peek: Char = {
    if (isAtEnd) '\0' else source.charAt(current)
  }

  private[this] def peekNext: Char = {
    if (current + 1 >= source.length) return '\0'
    source.charAt(current + 1)
  }

  private[this] def matchChar(expected: Char): Boolean = {
    if (isAtEnd) return false
    if (source.charAt(current) != expected) return false
    current += 1
    true
  }

  private[this] def string: Unit = {
    // find terminator
    while (peek != '"' && !isAtEnd) {
      if (peek == '\n') line += 1
      advance
    }

    // report missing terminator
    if (isAtEnd) return Lox.error(line, "Unterminated string.")

    // eat terminator
    advance

    val value = source.substring(start + 1, current - 1)
    addToken(STRING, value)
  }

  private[this] def number: Unit = {
    // keep eating numbers until end
    while (peek.isDigit) advance
    // look for fractional part
    if (peek == '.' && peekNext.isDigit) {
      advance
      while(peek.isDigit) advance
    }

    // convert to double
    val num = source.substring(start, current).toDouble.asInstanceOf[AnyRef]

    addToken(NUMBER, num)
  }

  private[this] def identifier: Unit = {
    while (isAlphaNumeric(peek)) advance
    // check for keyword
    val s = source.substring(start, current)
    val r = keywords.get(s)
    // decide whether it's a keyword or unique
    r match {
      case Some(v) => addToken(v)
      case None => addToken(IDENTIFIER)
    }
  }

}
