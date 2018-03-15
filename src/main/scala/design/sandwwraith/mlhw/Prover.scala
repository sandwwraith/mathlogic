package design.sandwwraith.mlhw

import scala.collection.{mutable => m}
import design.sandwwraith.mlhw.model._
import design.sandwwraith.mlhw.model.ExprTypes.EvalContext
import design.sandwwraith.mlhw.model.Results.{ErrorMessage, NotTrue, ProofFailure}
import design.sandwwraith.mlhw.util.Proofs

class Prover {
  def !!(e: Expr) = :!(e)

  def apply(expr: Expr): Either[ProofFailure, List[Expr]] = {
    whenFalse(expr) match {
      case Some(q) => Left(NotTrue(q))
      case _ => Right(makeNotAnnotatedProof(expr))
    }
  }

  private def whenFalse(expr: Expr): Option[EvalContext] = createVarsToBoolList(expr.vars).find(!expr.eval(_))

  private def gen(i: Int): List[List[Boolean]] = i match {
    case 0 => List()
    case 1 => List(List(false), List(true))
    case _ => gen(i - 1).flatMap(x => List(x :+ false, x :+ true))
  }

  private def createVarsToBoolList(vars: List[String]): List[EvalContext] = gen(vars.size).map(x => vars.zip(x).toMap)

  private def checkVars(first: EvalContext, second: EvalContext): Boolean = {
    for (i <- first) {
      second.get(i._1) match {
        case Some(a) => if (a != i._2) return false
        case _ => return false
      }
    }
    true
  }

  private def makeNotAnnotatedProof(expr: Expr): List[Expr] = {
    val curVarsList = expr.vars
    var curVars = m.HashSet[String]() ++ curVarsList
    var proofs = createVarsToBoolList(curVarsList)
      .map((hypot: EvalContext) => hypot -> Proofs.buildProof(expr)(hypot)).toMap
    while (curVars.nonEmpty) {
      val tp = removeAssumption(curVars, proofs, expr)
      curVars = tp._1
      proofs = tp._2
    }
    proofs.head._2
  }

  private def removeAssumption(vars: m.HashSet[String], proofs: Map[EvalContext, List[Expr]],
                       beta: Expr): (m.HashSet[String], Map[EvalContext, List[Expr]]) = {
    val smallerVars = vars.init
    val smallerEvalContextList = createVarsToBoolList(smallerVars.toList)
    var newProofs = Map[EvalContext, List[Expr]]()
    if (smallerVars.isEmpty) {
      var deduceContext0 = List[Expr]()
      var deduceContext1 = List[Expr]()
      if (proofs.head._1.head._2) {
        deduceContext0 ++= List(Term(proofs.head._1.head._1))
      } else {
        deduceContext0 ++= List(:!(Term(proofs.head._1.head._1)))
      }
      if (proofs.last._1.head._2) {
        deduceContext1 ++= List(Term(proofs.last._1.head._1))
      } else {
        deduceContext1 ++= List(:!(Term(proofs.last._1.head._1)))
      }
      (smallerVars, List(Map.empty[String, Boolean]
        -> mergeProofs(deduceContext0, deduceContext1, Term(vars.last), beta, proofs.head._2, proofs.last._2)).toMap)
    } else {
      for (i <- smallerEvalContextList) {
        var proofsToMerge = m.MutableList[List[Expr]]()
        var deduceContext = m.MutableList[List[Expr]]()
        var varToRemove = ""
        for (j <- proofs) {
          val hypot = j._1
          if (checkVars(i, hypot)) {
            proofsToMerge += j._2
            varToRemove = hypot.keySet.filterNot(i.keySet).head
            var context = List[Expr]()
            for (h <- hypot if !h._1.equals(varToRemove)) {
              val varAddToContext = Term(h._1)
              if (!h._2) {
                context ++= List(:!(varAddToContext))
              } else {
                context ++= List(varAddToContext)
              }
            }
            context ++= List(if (hypot(varToRemove)) Term(varToRemove) else :!(Term(varToRemove)))
            deduceContext += context
          }
        }
        if (proofsToMerge.size != 2)
          throw new Error("proofsToMerge.size != 2")
        if (varToRemove.isEmpty)
          throw new Error("varToRemove.isEmpty")
        newProofs += (i -> mergeProofs(deduceContext.head.toSeq, deduceContext(1).toSeq,
          Term(varToRemove), beta, proofsToMerge.head, proofsToMerge(1)))
      }
      (smallerVars, newProofs)
    }
  }


  private def mergeProofs(firstDeduceContext: Seq[Expr], secondDeduceContext: Seq[Expr], varToRemove: Term, beta: Expr,
                  firstProof: List[Expr], secondProof: List[Expr]): List[Expr] = {
    var newProof = List[Expr]()
    val deduced0 = Deducer(firstProof, firstProof.last, firstDeduceContext) match {
      case Left(error) => throw new Error("wrong proof in deduce0 : " + error);
      case Right(proof) => proof._3
    }
    val deduced1 = Deducer(secondProof, secondProof.last, secondDeduceContext) match {
      case Left(error) => throw new Error("wrong proof in deduce1 : " + error);
      case Right(proof) => proof._3
    }
    newProof ++= (deduced0 ++ deduced1 ++ Proofs.tertiumNonDatur(varToRemove))
    newProof ++= List[Expr](
      (varToRemove ->: beta) ->: (:!(varToRemove) ->: beta) ->: (varToRemove V :!(varToRemove)) ->: beta,
      (:!(varToRemove) ->: beta) ->: ((varToRemove V :!(varToRemove)) ->: beta),
      (varToRemove V :!(varToRemove)) ->: beta,
      beta)
    newProof
  }

}
