package design.sandwwraith.mlhw

import design.sandwwraith.mlhw.model.Results.ParsingException
import design.sandwwraith.mlhw.model._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

class ParserSpec extends FlatSpec with Matchers {
  private def parse(s: String): Either[ParsingException, Expr] = {
    val parser = new ExpressionParser(s.replaceAll(" ", ""))
    parser.inputLine.run().toEither.left.map(ParsingException(_, parser))
  }

  private def mustBeParsed(result: Either[ParsingException, Expr]) = result match {
    case Right(_) =>
    case Left(f) => assert(false, s"Parser must completed normally, but emitted error: $f")
  }

  "Expression parser" should "parse a&b->c" in {
    parse("A&B->C") match {
      case Right(_) => assert(true)
      case Left(f) => assert(false, s"Parser must completed normally, but emitted error: $f")
    }
  }

  "Expression parser with digits and predicates" should "parse @a(a+0'=a)" in {
    mustBeParsed(parse("@a(a+0'=a)"))
  }
}
