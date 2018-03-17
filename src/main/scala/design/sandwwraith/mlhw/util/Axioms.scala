package design.sandwwraith.mlhw.util

import design.sandwwraith.mlhw.model.Results._
import design.sandwwraith.mlhw.model._

object Axioms {

  private val A = Term("a")
  private val B = Term("b")
  private val C = Term("c")
  private val Z = Term("0")

  def axiomNumber(expr: Expr, line: Int): Either[ProofFailure, Option[Axiom]] = expr match {
    case a -> (_ -> b) if a == b => Right(Some(Axiom(1)))
    case (a -> b) -> ((c -> (d -> e)) -> (f -> g)) if a == c && b == d && a == f && e == g => Right(Some(Axiom(2)))
    case a -> (b -> (c :& d)) if a == c && b == d => Right(Some(Axiom(3)))
    case (a :& _) -> b if a == b => Right(Some(Axiom(4)))
    case (_ :& a) -> b if a == b => Right(Some(Axiom(5)))
    case a -> (b :| _) if a == b => Right(Some(Axiom(6)))
    case a -> (_ :| b) if a == b => Right(Some(Axiom(7)))
    case (a -> b) -> ((c -> d) -> ((e :| f) -> g)) if a == e && b == d && b == g && c == f => Right(Some(Axiom(8)))
    case (a -> b) -> ((c -> :!(d)) -> :!(e)) if a == c && a == e && b == d => Right(Some(Axiom(9)))
    case :!(:!(a)) -> b if a == b => Right(Some(Axiom(10)))
    case ->(FA(x, e), phi) if e.findChanges(x, phi).isDefined =>
      if (e.isSubstituted(x, phi))
        Right(Some(Axiom(11)))
      else
        Left(NotFreeForSubstitution(e.findChanges(x,phi).get.headOption.getOrElse(x), x, e, line))
    case ->(phi, EX(x, e)) if e.findChanges(x, phi).isDefined =>
      if (e.isSubstituted(x, phi))
        Right(Some(Axiom(12)))
      else
        Left(NotFreeForSubstitution(e.findChanges(x,phi).get.headOption.getOrElse(x), x, e, line))
    case ->(Predicate("=", Seq(`A`, `B`)), Predicate("=", Seq(Term("'", Seq(`A`)), Term("'", Seq(`B`))))) => Right(Some(Axiom(13)))
    case ->(Predicate("=", Seq(`A`, `B`)), ->(Predicate("=", Seq(`A`, `C`)), Predicate("=", Seq(`B`, `C`)))) => Right(Some(Axiom(14)))
    case ->(Predicate("=", Seq(Term("'", Seq(`A`)), Term("'", Seq(`B`)))), Predicate("=", Seq(`A`, `B`))) => Right(Some(Axiom(15)))
    case :!(Predicate("=", Seq(Term("'", Seq(`A`)), `Z`))) => Right(Some(Axiom(16)))
    case Predicate("=", Seq(Term("+", Seq(`A`, Term("'", Seq(`B`)))), Term("'", Seq(Term("+", Seq(`A`, `B`)))))) => Right(Some(Axiom(16)))
    case Predicate("=", Seq(Term("+", Seq(`A`, `Z`)), `A`)) => Right(Some(Axiom(17)))
    case Predicate("=", Seq(Term("*", Seq(`A`, `Z`)), `Z`)) => Right(Some(Axiom(18)))
    case Predicate("=", Seq(Term("*", Seq(`A`, Term("'", Seq(`B`)))), Term("+", Seq(Term("*", Seq(`A`, `B`)), `A`)))) => Right(Some(Axiom(19)))
    case ->(:&(phi, FA(x, ->(psi, xi))), theta) if {
      psi == theta && psi.varEntersFree(x) &&
        psi.substitute(Map(x.toString -> Term("0"))) == phi &&
        psi.substitute(Map(x.toString -> Term("'", Seq(x)))) == xi
    } => Right(Some(Axiom(20)))
    case _ => Right(None)
  }
}
