package scalox

trait Expr
case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
case class Unary(operator: Token, right: Expr) extends Expr
case class Literal(value: AnyRef) extends Expr
case class Grouping(expr: Expr) extends Expr
