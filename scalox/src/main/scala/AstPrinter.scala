package scalox

// pattern matching amirite

object AstPrinter {
  def print(e: Expr): String = e match {
    case Binary(l, op, r) => parens(op.lexeme, l, r)
    case Unary(op, r) => parens(op.lexeme, r)
    case Literal(v) => s"$v"
    case Grouping(e) => s"(${print(e)})"
    case _ => "unimplemented"
  }

  private[this] def parens(name: String, expr: Expr*): String = {
    var out = s"($name"
    expr.map((e: Expr) => {
      out = out + s" ${print(e)}"
    })
    out = out + ")"
    return out
  }
}
