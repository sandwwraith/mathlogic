package design.sandwwraith.mlhw

import design.sandwwraith.mlhw.model._
import org.parboiled2._

class ExpressionParser(val input: ParserInput) extends Parser {

  def lineWithContext: Rule1[(Seq[Expr], Expr)] = rule {
    ((zeroOrMore(expression).separatedBy(",") ~> ((a: Seq[Expr]) => a.toList)) ~
      "|-" ~
      expression) ~> ((a: List[Expr], b: Expr) => (a, b)) ~
      EOI
  }

  def inputLine = rule {
    expression ~ EOI
  }

  private def expression: Rule1[Expr] = rule { disjunction ~ zeroOrMore("->" ~ expression ~> ->) }
  private def disjunction: Rule1[Expr] = rule { conjunction ~ zeroOrMore("|" ~ conjunction ~> :|) }
  private def conjunction: Rule1[Expr] = rule { unary ~ zeroOrMore("&" ~ unary ~> :&) }

  private def unary: Rule1[Expr] = rule { predicate | negation | parens | ("@" ~ variable ~ unary ~> ((a, b) => FA(a, b))) |
    ("?" ~ variable ~ unary ~> ((a, b) => EX(a, b))) }

  def predicate: Rule1[Expr] = rule {
    (term ~ "=" ~ term ~> ((a: Term, b: Term) => Predicate("=", Seq(a, b)))
    | capture(oneOrMore(CharPredicate.UpperAlpha) ~ zeroOrMore(CharPredicate.AlphaNum)) ~ optional("(" ~ zeroOrMore(term).separatedBy(",") ~ ")") ~>
      ((a: String, b:Option[Seq[Term]]) => if (b.isEmpty) Term(a) else Predicate(a, b.get))
    | term)
  }
  def term: Rule1[Term] =
    leftAssoc(summable, (a: Term, b: Term) => Term("+", Seq(a, b)), "+")
  private def summable: Rule1[Term] =
    leftAssoc(mullable, (a: Term, b: Term) => Term("*", Seq(a, b)), "*")
  private def mullable: Rule1[Term] =
    rule {
      ((capture(CharPredicate.LowerAlpha) ~
        "(" ~
        oneOrMore(term).separatedBy(",") ~
        ")" ~>
        ((a: String, b: Seq[Term]) => Term(a, b)))
        | variable
        | ("(" ~ term ~ ")")
        | (str("0") ~> (() => Term("0")))) ~
        zeroOrMore(capture("'")) ~> ((a: Term, b: Seq[_]) => wrapInQuote(a, b.length)) }

  private def leftAssoc[A](a: => Rule1[A], b: (A, A) => A, divider: String): Rule1[A]
  = rule { a ~ zeroOrMore(divider ~ a ~> b) }

  private def wrapInQuote(e: Term, n: Int): Term = {
    if (n < 1) e else wrapInQuote(Term("'", List(e)), n - 1)
  }

  private def negation: Rule1[Expr] = rule { "!" ~ unary ~> :! }
  private def variable: Rule1[Term] = rule { capture(letters) ~> ((a: String) => Term(a)) }
  private def letters: Rule0 = rule { oneOrMore(CharPredicate.LowerAlpha) ~ zeroOrMore(CharPredicate.Digit) }
  private def parens: Rule1[Expr] = rule { "(" ~ expression ~ ")" }
}
