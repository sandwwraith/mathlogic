package design.sandwwraith.mlhw

import design.sandwwraith.mlhw.Deducer.DeductionResult
import design.sandwwraith.mlhw.Runner.parse
import design.sandwwraith.mlhw.model.Expr
import design.sandwwraith.mlhw.model.Results._

import scala.io.Source
import scala.util.{Failure, Success}

object Runner {
  def parse(s: Iterator[String]): Either[ProofFailure, Seq[Expr]] = (s filterNot (_.trim.isEmpty) foldLeft Right(Seq()).asInstanceOf[Either[ProofFailure, Seq[Expr]]]) ((acc, input) => {
    acc.flatMap((col: Seq[Expr]) => {
      val parser = new ExpressionParser(input.replaceAll(" ", ""))
      parser.inputLine.run() match {
        case Success(p) => Right(col :+ p)
        case Failure(ex) => return Left(ParsingException(ex, parser))
      }
    })
  })

  def getSource(folder: String)(implicit args: Array[String]): Option[Source] = {
    if (args.isEmpty) None
    else Some(Source.fromFile(s"testdata/$folder/${args(0)}"))
  }

  def runMethod[R](folder: String)
                  (method: (Source) => Either[ProofFailure, R], formatter: (R) => String, checker: => (R) => String)
                  (implicit args: Array[String]) : Unit = {
    getSource(folder).map((s: Source) => method(s) match {
      case Left(failure) => failure.toString
      case Right(r) => if (args.contains("andCheck")) checker(r) else formatter(r)
    }) match {
      case None => System.err.println(s"No input file provided. Pass filename from folder testdata/$folder")
      case Some(s) => println(s)
    }
  }
}


object Task1 extends App {

  override implicit val args: Array[String] = super.args

  private def runProof(prover: Checker)(s: Source): Either[ProofFailure, Proof] = {
    val lines: Seq[String] = s getLines() filterNot (_.trim.isEmpty) map (_.replaceAll(" ", "")) toSeq;
    if (lines.isEmpty) return Right(new Proof())
    val parser = new ExpressionParser(lines.head)
    val (ctx, flag) = parser.lineWithContext.run() match {
      case Success((c, _)) => (c, true)
      case Failure(_) => (List.empty, false)
    }
    Runner.parse((if (flag) lines.tail else lines).iterator).flatMap(prover(_, ctx))
  }

  Runner.runMethod("HW1")(runProof(new Checker()), (r: Proof) => r.mkString("\n"), throw new RuntimeException("already checked"))
}

object Task2 extends App {
  override implicit val args: Array[String] = super.args

  private def deduceProof(deducer: Deducer)(s: Source): Either[ProofFailure, DeductionResult] = {
    val lines = s.getLines() filterNot (_.trim.isEmpty)
    if (lines.isEmpty) return Left(ErrorMessage("Пустой заголовок недопустим"))
    val parser = new ExpressionParser(lines.next().replaceAll(" ", ""))
    val (ctx, beta) = parser.lineWithContext.run() match {
      case Success(t) => t
      case Failure(ex) => return Left(ParsingException(ex, parser))
    }
    if (ctx.isEmpty) return Left(ErrorMessage("Отсутствует альфа"))
    parse(lines).flatMap(deducer(_, beta, ctx))
  }

  Runner.runMethod("HW2")(deduceProof(new Deducer()),
    (p: DeductionResult) => p._1.mkString(", ") + "|-" + p._2.toString + "\n" + p._3.mkString("\n"),
    (p: DeductionResult) => {val (ctx, beta, proof) = p; ctx.mkString(", ") + "|-" + beta.toString + "\n" + Checker(proof, ctx).right.get.mkString("\n") }
  )
}

object Task3 extends App {
  override implicit val args: Array[String] = super.args

  private def makeProof(prover: Prover)(s: Source): Either[ProofFailure, List[Expr]] = {
    val lines = s.getLines() filterNot (_.trim.isEmpty)
    if (lines.isEmpty) return Left(ErrorMessage("Пустой ввод недопустим"))
    val parser = new ExpressionParser(lines.next().replaceAll(" ", ""))
    val input = parser.inputLine.run() match {
      case Success(t) => t
      case Failure(ex) => return Left(ParsingException(ex, parser))
    }
    prover(input)
  }

  Runner.runMethod("HW3")(makeProof(new Prover()), (p: Seq[Expr]) => p.mkString("\n"), (p: Seq[Expr]) => Checker(p).right.get.mkString("\n"))
}
