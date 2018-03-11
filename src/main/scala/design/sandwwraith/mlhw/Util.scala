package design.sandwwraith.mlhw

import design.sandwwraith.mlhw.model.Results._
import design.sandwwraith.mlhw.model.ExprTypes._
import design.sandwwraith.mlhw.model._

object Util {
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
    case _ => Right(None)
  }
}
