package design.sandwwraith.mlhw

import design.sandwwraith.mlhw.Deducer.DeductionResult
import design.sandwwraith.mlhw.model._
import design.sandwwraith.mlhw.model.Results._
import design.sandwwraith.mlhw.util.Deductions

import scala.collection.mutable

object Deducer {
  type DeductionResult = (Seq[Expr], Expr, Seq[Expr])

  def apply(proof: Seq[Expr], beta: Expr, context: Seq[Expr] = List.empty): Either[ProofFailure, DeductionResult]
    = new Deducer().apply(proof, beta, context)
}

class Deducer {

  private def case1(di: Statement)(implicit alpha: Expr): List[Expr] = {
    val line1 = di.expr ->: (alpha ->: di.expr)
    List(di.expr, line1, alpha ->: di.expr)
  }

  private def case2(di: Statement)(implicit alpha: Expr): List[Expr] = {
    val line1 = alpha ->: (alpha ->: alpha)
    val line2 = line1 ->: (alpha ->: ((alpha ->: alpha) ->: alpha)) ->: (alpha ->: alpha)
    val line3 = line2.right
    val line4 = alpha ->: ((alpha ->: alpha) ->: alpha)
    val line5 = alpha ->: alpha
    List(line1, line2, line3, line4, line5)
  }

  private def case3(di: Statement, dj: Statement)(implicit alpha: Expr): List[Expr] = {
    val line1 = (alpha ->: dj.expr) ->: ((alpha ->: (dj.expr ->: di.expr)) ->: (alpha ->: di.expr))
    val line2 = line1.right
    val line3 = alpha ->: di.expr
    List(line1, line2, line3)
  }

  def deduceFA(what:Expr, alpha: Expr): List[Expr] = {
    what match {
      case ->(phi, FA(x, psi)) =>
        Deductions.lemma1FA(alpha, phi, FA(x, psi)) ++
          Deductions.lemma2FA(alpha, phi, psi) ++
          List(
            alpha ->: (phi ->: psi),
            (alpha & phi) ->: psi,
            (alpha & phi) ->: FA(x, psi),
            alpha ->: (phi ->: FA(x, psi))
          )
      case _ => throw new IllegalArgumentException("wtf")
    }
  }

  private def caseFA(st: Statement, alpha: Expr): List[Expr] = {
    deduceFA(st.expr, alpha)
  }

  def deduceEX(what: Expr, alpha: Expr): List[Expr] = {
    what match {
      case ->(EX(x, psi), phi) =>
        Deductions.lemmaEX(alpha, psi, phi) ++
          List(alpha ->: (psi ->: phi),
            psi ->: (alpha ->: phi),
            EX(x, psi) ->: (alpha ->: phi)) ++
          Deductions.lemmaEX(EX(x, psi) , alpha, phi) ++
          List(alpha ->: (EX(x, psi) ->: phi))
      case _ => throw new IllegalArgumentException("wtf")
    }
  }

  private def caseEX(st: Statement, alpha: Expr): List[Expr] = {
    deduceEX(st.expr, alpha)
  }

  private def compile(proof: Proof, beta: Expr, context: Seq[Expr]): Either[ProofFailure, DeductionResult] = {
    implicit val alpha: Expr = context.last
    val compiledProof = new mutable.MutableList[Expr]()

    proof.foreach((st: Statement) => {
      val q = st match {
        case Statement(_, e, _) if e == alpha => case2(st)
        case Statement(_, e, a) if e != alpha =>
          a match {
            case Axiom(_) => case1(st)
            case Assumption() => case1(st)
            case MP(j: Statement, k: Statement) => case3(st, j)
            case InferFA(_) => caseFA(st, alpha)
            case InferEX(_) => caseEX(st, alpha)
          }
      }
      compiledProof ++= q
    })
    Right((context.init, alpha ->: beta, compiledProof))
  }

  def apply(proof: Seq[Expr], beta: Expr, context: Seq[Expr] = List.empty): Either[ProofFailure, DeductionResult] = {
    new Checker()(proof, context) flatMap(compile(_, beta, context))
  }

  // bridge method
  def apply(context: Seq[Expr], beta: Option[Expr], proof: Seq[Expr]): Either[ProofFailure, DeductionResult] = apply(proof, beta.get, context)
}
