package design.sandwwraith.mlhw

import design.sandwwraith.mlhw.model._
import design.sandwwraith.mlhw.util.Proofs2
import design.sandwwraith.mlhw.util.Proofs2.parseUnsafe

object Peano {
  def apply(a: Int, b: Int): Seq[Expr] = new Peano().genProof(a, b)
}

class Peano {
  private val Zero = Term("0", Seq.empty)

  def numToPeano(num: Int): Term = {
    require(num >= 0)
    ExpressionParser.wrapInQuote(Zero, num)
  }

  def peanoToNum(term: Term): Int = term match {
    case Term("'", args) => 1 + peanoToNum(args.head)
    case Term("0", Nil) => 0
    case _ => throw new IllegalArgumentException("Not a peano number")
  }

  def genProof(a: Int, b: Int): Seq[Expr] = {
    if (b == 0) return genZeroProof(a)
    else ???
  }

  def genZeroProof(a: Int): Seq[Expr] = {
    (Proofs2.genA0A(numToPeano(a)) ++ base).reverse
  }

  private lazy val base = parseUnsafe(Seq("A&B->B", "a+0=a"))
}
