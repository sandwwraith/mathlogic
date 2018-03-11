package design.sandwwraith.mlhw

import design.sandwwraith.mlhw.model._
import org.parboiled2._

class ExpressionParser(val input: ParserInput) extends Parser {
  def inputLine = rule {
    expression ~ EOI
  }

  private def expression: Rule1[Expr] = rule { disjunction ~ zeroOrMore("->" ~ expression ~> ->) }
  private def disjunction: Rule1[Expr] = rule { conjunction ~ zeroOrMore("|" ~ conjunction ~> :|) }
  private def conjunction: Rule1[Expr] = rule { unary ~ zeroOrMore("&" ~ unary ~> :&) }

  private def unary: Rule1[Expr] = rule { variable | negation | parens }

  private def negation: Rule1[Expr] = rule { "!" ~ unary ~> :! }
  private def letters: Rule0 = rule { oneOrMore(CharPredicate.UpperAlpha) ~ zeroOrMore(CharPredicate.Digit) }
  private def parens: Rule1[Expr] = rule { "(" ~ expression ~ ")" }

  private def variable: Rule1[Term] = rule { capture(letters) ~> Term }
}
