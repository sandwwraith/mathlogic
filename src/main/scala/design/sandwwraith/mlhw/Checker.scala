package design.sandwwraith.mlhw

import design.sandwwraith.mlhw.model.Results._
import design.sandwwraith.mlhw.model._
import design.sandwwraith.mlhw.util.Axioms

import scala.collection.{mutable => m}

object Checker {
  def apply(proof: Seq[Expr], context: Seq[Expr] = List.empty): Either[ProofFailure, Proof] = new Checker().apply(proof, context)
}

class Checker {
  private val reversedImplications = new m.HashMap[Expr, m.Set[(Expr, Int)]]() with m.MultiMap[Expr, (Expr, Int)]
  private var curLine: Int = 0

  def apply(proof: Seq[Expr], context: Seq[Expr] = List.empty): Either[ProofFailure, Proof] = {
    implicit val compiledProof: Proof = new Proof()
    proof.foreach((expr) => {
      expr match {
        case a -> b => reversedImplications.addBinding(b, (a, curLine))
        case _ =>
      }
      Axioms.axiomNumber(expr, curLine) match {
        case Left(r) => return Left(r)
        case Right(Some(axiom)) => compiledProof += Statement(curLine, expr, axiom)
        case _ if context.contains(expr) => compiledProof += Statement(curLine, expr, Assumption())
        case _ => findMP(expr) match {
          case None => return Left(WrongProofFromLine(curLine, "Невыводимое выражение"))
          case Some(mp) => compiledProof += Statement(curLine, expr, mp)
        }
      }
      curLine += 1
    })
    Right(compiledProof)
  }

  def findMP(expr: Expr)(implicit proof: Proof): Option[MP] = {
    val candidates = reversedImplications.getOrElse(expr, Set())
    for (c <- candidates;
         p <- proof
         if p.expr.equals(c._1)) {
      return Some(MP(p.line, c._2))
    }
    None
  }
}

