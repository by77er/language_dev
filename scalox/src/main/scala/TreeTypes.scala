package scalox

trait Expr
case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
