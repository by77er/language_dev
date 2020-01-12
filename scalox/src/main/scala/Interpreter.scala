package scalox

// still going to pattern match instead of using the visitor pattern

import TokenType._

class Interpreter {
  def interpret(expression: Expr): Unit = {
    try {
      val out = evaluate(expression)
      println(out)
    } catch {
      case re: RuntimeError => Lox.runtimeError(re)
    }
  }

  private[this] def evaluate(expr: Expr): AnyRef = expr match {
    case b: Binary => binaryExpr(b)
    case u: Unary => unaryExpr(u)
    case l: Literal => literalExpr(l)
    case g: Grouping => groupingExpr(g)
  }

  private[this] def literalExpr(expr: Literal): AnyRef = {
    expr.value
  }

  private[this] def groupingExpr(expr: Grouping): AnyRef = {
    evaluate(expr.expr)
  }

  private[this] def unaryExpr(expr: Unary): AnyRef = {
    val right = evaluate(expr.right)
    expr.operator.kind match {
      case MINUS => {
        checkNumberOperand(expr.operator, right)
        return (-right.asInstanceOf[Double]).asInstanceOf[AnyRef]
      }
      case BANG  => return (!isTruthy(right)).asInstanceOf[AnyRef]
    }
    // Unreachable
    null
  }

  private[this] def checkNumberOperand(operator: Token, operand: AnyRef): Unit = {
    if (operand.isInstanceOf[Double]) return;
    throw new RuntimeError(operator, "Operand must be a number")
  }

  private[this] def checkNumberOperands(operator: Token, operand1: AnyRef, operand2: AnyRef): Unit = {
    if (operand1.isInstanceOf[Double] && operand2.isInstanceOf[Double]) return;
    throw new RuntimeError(operator, "Operands must both be numbers")
  }

  private[this] def isTruthy(ref: AnyRef): Boolean = {
    if (ref == null) return false
    if (ref.isInstanceOf[Boolean]) return ref.asInstanceOf[Boolean]
    return true;
  }

  private[this] def binaryExpr(expr: Binary): AnyRef = {
    // Post-order traversal
    val left = evaluate(expr.left)
    val right = evaluate(expr.right)

    return (expr.operator.kind match {
      // comparison
      case GREATER => {
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] > right.asInstanceOf[Double]
      }
      case GREATER_EQUAL => {
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] >= right.asInstanceOf[Double]
      }
      case LESS => {
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] < right.asInstanceOf[Double]
      }
      case LESS_EQUAL => {
        checkNumberOperands(expr.operator, left, right)
        left.asInstanceOf[Double] <= right.asInstanceOf[Double]
      }
      // equality
      case BANG_EQUAL => !isEqual(left, right)
      case EQUAL_EQUAL => isEqual(left, right)
      // arithmetic
      case MINUS => left.asInstanceOf[Double] - right.asInstanceOf[Double]
      case SLASH => left.asInstanceOf[Double] / right.asInstanceOf[Double]
      case STAR  => left.asInstanceOf[Double] * right.asInstanceOf[Double]
      case PLUS  => {
        if (left.isInstanceOf[Double] && right.isInstanceOf[Double]) {
          left.asInstanceOf[Double] + right.asInstanceOf[Double]
        } else if (left.isInstanceOf[String] && right.isInstanceOf[String]) {
          left.asInstanceOf[String] + right.asInstanceOf[String]
        } else {
          throw new RuntimeError(expr.operator, "Operands must be two numbers or two strings.")
        }

        
      }
      case _ => null
    }).asInstanceOf[AnyRef]
  }

  private[this] def isEqual(left: AnyRef, right: AnyRef): Boolean = {
    if (left == null && right == null) {
      true
    } else if (left.isInstanceOf[Boolean] && right.isInstanceOf[Boolean]) {
      left.asInstanceOf[Boolean] == right.asInstanceOf[Boolean]
    } else if (left.isInstanceOf[Double] && right.isInstanceOf[Double]) {
      left.asInstanceOf[Double] == right.asInstanceOf[Double]
    } else if (left.isInstanceOf[String] && right.isInstanceOf[String]) {
      left.asInstanceOf[String] == right.asInstanceOf[String]
    } else {
      false
    }
  }

}

class RuntimeError(val token: Token, val message: String) extends Exception(message)
