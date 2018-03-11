package design.sandwwraith.mlhw

import design.sandwwraith.mlhw.model.Expr
import design.sandwwraith.mlhw.model.Results.{ParsingException, Proof, ProofFailure}

import scala.io.Source
import scala.util.{Failure, Success}

object Runner {
  def runProof(s: Source)(prover: (Seq[Expr]) => Either[ProofFailure, Proof]): Either[ProofFailure, Proof] = {
    val parsed = (s getLines() filterNot (_.trim.isEmpty) foldLeft Right(Seq()).asInstanceOf[Either[ProofFailure, Seq[Expr]]]) ((acc, input) => {
      acc.flatMap((col: Seq[Expr]) => {
        val parser = new ExpressionParser(input.replaceAll(" ", ""))
        parser.inputLine.run() match {
          case Success(p) => Right(col :+ p)
          case Failure(ex) => Left(ParsingException(ex, parser))
        }
      })
    })
    parsed.flatMap(prover)
  }
}


object Task1 extends App {

  val str1 = "a&b->a\nb&a->b"
  val str2 =
    """
      |A->B->A
      |((A))->(A->A)->(((A)))
      |A->(((((A)->A))))
      |(A->A->A)->(A->(A->A)->A)->(A->A)
      |(A->(A->A)->A)->(A->A)
      |A->A
    """.stripMargin

  val source = if (args.isEmpty) Source.fromString(str2)
  else Source.fromFile(s"testdata/HW1/${args(0)}")

  val result = Runner.runProof(source) {
    new Checker().apply(_)
  } match {
    case Left(failure) => failure.toString
    case Right(proof) => proof.mkString("\n")
  }

  println(result)
}

