package design.sandwwraith.mlhw.model

abstract sealed class Expr(val symbol: String) {
  def ->:(other: Expr) = ->(other, this)
}

abstract sealed class BinaryExpr(val left: Expr, val right: Expr, symbol: String) extends Expr(symbol) {
  override def toString: String = s"($left $symbol $right)"
}


case class ->(private val a: Expr, private val b: Expr) extends BinaryExpr(a, b, "->")

case class :|(private val a: Expr, private val b: Expr) extends BinaryExpr(a, b, "|")

case class :&(private val a: Expr, private val b: Expr) extends BinaryExpr(a, b, "&")


case class :!(expr: Expr) extends Expr("!") {
  override def toString: String = s"!$expr"
}

case class Term(content: String) extends Expr(content) {
  override def toString: String = content
}


object ExprTypes {
  type Impl = ->
  type Disj = :|
  type Conj = :&
  type Neg = :!
}

