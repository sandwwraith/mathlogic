package design.sandwwraith.mlhw

import design.sandwwraith.mlhw.model._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

class ParserSpec extends FlatSpec with Matchers {
  private def parse(s: String) = new ExpressionParser(s.replaceAll(" ", "")).inputLine.run()

  "Expression parser" should "parse a&b->c" in {
    parse("a&b->c") match {
      case Success((Term("a") :& Term("b")) -> Term("c")) => assert(true)
      case _ => assert(false, "Did not match")
    }
  }
}
