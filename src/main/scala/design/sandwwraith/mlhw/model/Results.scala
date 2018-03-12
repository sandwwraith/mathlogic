package design.sandwwraith.mlhw.model

import design.sandwwraith.mlhw.model.ExprTypes.EvalContext
import org.parboiled2.{ParseError, Parser}

import scala.collection.mutable.MutableList

object Results {
  type Proof = MutableList[Statement]

  case class Statement(line: Int, expr: Expr, annotation: Annotation) {
    override lazy val toString: String = s"($line) $expr ($annotation)"
  }

  trait Annotation

  case class Axiom(number: Int) extends Annotation {
    override lazy val toString = "Сх. акс. " + number
  }

  case class Assumption() extends Annotation {
    override lazy val toString = "Предположение"
  }

  case class MP(first: Statement, second: Statement) extends Annotation {
    override lazy val toString = "M.P. " + first.line + ", " + second.line
  }

  trait ProofFailure

  case class NotTrue(values: EvalContext) extends ProofFailure {
    override lazy val toString: String = "Высказывание ложно при " + values.map(e => s"${e._1}=${if (e._2) "И" else "Л"}").mkString(",")
  }

  case class ParsingException(exception: Throwable, parser: Parser = null) extends ProofFailure {
    private val detailedError: String = if (parser != null && exception.isInstanceOf[ParseError]) exception.asInstanceOf[ParseError].format(parser) else null

    override def toString: String = "Входной файл содержит ошибки: " + (if (detailedError != null) detailedError else exception.toString)
  }

  case class WrongProofFromLine(lineNumber: Int, msg:String = "") extends ProofFailure {
    override lazy val toString = s"Доказательство неверно со строки $lineNumber${if (msg != "") " (" + msg + ")"}"
  }

  case class ErrorMessage(message: String) extends ProofFailure {
    override def toString: String = message
  }
}
