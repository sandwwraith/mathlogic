package design.sandwwraith.mlhw

import design.sandwwraith.mlhw.model.Results._
import design.sandwwraith.mlhw.model._
import design.sandwwraith.mlhw.util.Axioms

import scala.collection.{mutable => m}

object Checker {
  def apply(proof: Seq[Expr], context: Seq[Expr] = List.empty): Either[ProofFailure, Proof] = new Checker().apply(proof, context)
}

class Checker {
  type Context = Seq[Expr]

  private val reversedImplications = new m.HashMap[Expr, m.Set[(Expr, Int)]]() with m.MultiMap[Expr, (Expr, Int)]
  private var curLine: Int = 1

  def apply(proof: Seq[Expr], context: Seq[Expr] = List.empty): Either[ProofFailure, Proof] = {
    implicit val compiledProof: Proof = new Proof()
    implicit val ctx: Context = context
    proof.foreach((expr) => {
      expr match {
        case a -> b => reversedImplications.addBinding(b, (a, curLine))
        case _ =>
      }
      Axioms.axiomNumber(expr, curLine) match {
        case Left(r) => return Left(r)
        case Right(Some(axiom)) => compiledProof += Statement(curLine, expr, axiom)
        case _ if context.contains(expr) => compiledProof += Statement(curLine, expr, Assumption())
        case _ => getMPAnnotation(expr) match {
          case Right(mp) => compiledProof += Statement(curLine, expr, mp)
          case Left(l) => return Left(l)
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
      return Some(MP(p.line - 1, c._2 - 1))
    }
    None
  }

  private def getMPAnnotation(expr: Expr)(implicit proof: Proof, context: Context): Either[ProofFailure, Annotation] = findMP(expr) match {
    case Some(mp) =>
      Right(mp)
    case None =>
      expr match {
        case ->(phi, FA(x, psi)) if isProved(phi ->: psi) =>
          if (context.nonEmpty && context.last.varEntersFree(x))
            return Left(InferenceRuleOnFreeVar(x, context.last, curLine))
          if (phi.varEntersFree(x)) {
            Left(EntersFreely(x, expr, curLine))
          } else {
            Right(InferFA(lineInProof(expr).getOrElse(0)))
          }
        case ->(EX(x, psi), phi) if isProved(psi ->: phi) =>
          if (context.nonEmpty && context.last.varEntersFree(x))
            return Left(InferenceRuleOnFreeVar(x, context.last, curLine))
          if (phi.varEntersFree(x)) {
            Left(EntersFreely(x, expr, curLine))
          } else {
            Right(InferEX(lineInProof(expr).getOrElse(0)))
          }
        case _ =>
          Left(WrongProofFromLine(curLine, "Выражение не может быть выведено"))
      }
  }

  private def lineInProof(expr: Expr)(implicit proof: Proof): Option[Int] = {
    for (p <- proof if p.expr.equals(expr) && p.annotation.getClass != ProofError.getClass)
      return Some(p.line)
    None
  }

  private def isProved(expr: Expr)(implicit context: Context, proof: Proof) =
    lineInProof(expr).isDefined || context.contains(expr)
}

