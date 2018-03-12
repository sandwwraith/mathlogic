package design.sandwwraith.mlhw.model

import design.sandwwraith.mlhw.model.ExprTypes.{EvalContext, Neg}

abstract sealed class Expr(val symbol: String) {
  def ->:(other: Expr) = ->(other, this)
  def V(other: Expr): Expr = :|(this, other)

  def &(other: Expr): Expr = :&(this, other)

  def !!(other: Expr): Expr = :!(this)

  val vars: List[String]

  def eval(implicit ctx: EvalContext): Boolean
}

abstract sealed class BinaryExpr(val left: Expr, val right: Expr, symbol: String) extends Expr(symbol) {
  override def toString: String = s"($left $symbol $right)"

  override lazy val vars: List[String] = left.vars ++ right.vars
}


case class ->(private val a: Expr, private val b: Expr) extends BinaryExpr(a, b, "->") {
  def eval(implicit ctx: EvalContext): Boolean = !a.eval || b.eval
}

case class :|(private val a: Expr, private val b: Expr) extends BinaryExpr(a, b, "|") {
  def eval(implicit ctx: EvalContext): Boolean = a.eval || b.eval
}

case class :&(private val a: Expr, private val b: Expr) extends BinaryExpr(a, b, "&") {
  def eval(implicit ctx: EvalContext): Boolean = a.eval && b.eval
}


case class :!(expr: Expr) extends Expr("!") {
  override def toString: String = s"!$expr"

  override lazy val vars: List[String] = expr.vars

  def eval(implicit ctx: EvalContext): Boolean = !expr.eval
}

case class Term(content: String) extends Expr(content) {
  override def toString: String = content

  override lazy val vars: List[String] = List(content)

  def eval(implicit ctx: EvalContext): Boolean = ctx(content)
}


object ExprTypes {
  type Impl = ->
  type Disj = :|
  type Conj = :&
  type Neg = :!

  type EvalContext = Map[String, Boolean]
}

