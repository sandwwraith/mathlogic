package design.sandwwraith.mlhw

import design.sandwwraith.mlhw.model.Results.Proof
import design.sandwwraith.mlhw.model.{Expr, Results}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class HWSpec extends FlatSpec with Matchers {
  "Homework 3" should "work on A->A" in {
    val input = new ExpressionParser("A->A").inputLine.run().get
    val proved = new Prover().apply(input).right.get
    val verified: Either[Results.ProofFailure, Proof] = new Checker().apply(proved)
    verified match {
      case Right(_) =>
      case Left(failure) => assert(false, s"Should be proved but was failure: $failure")
    }
  }

}
