package design.sandwwraith.mlhw.util

import design.sandwwraith.mlhw.ExpressionParser
import design.sandwwraith.mlhw.model.ExprTypes.toTerm
import design.sandwwraith.mlhw.model._

object Proofs2 {
  def parseUnsafe(s: Seq[String]): Seq[Expr] = {
    s.map(new ExpressionParser(_).inputLine.run().get)
  }

  type Replacement = (Term, Term) // from, to
  def unsafeReplaceTerm(in: Expr)(implicit replacement: Replacement): Expr = in match {
    case ->(a, b) => ->(unsafeReplaceTerm(a), unsafeReplaceTerm(b))
    case :|(a, b) => :|(unsafeReplaceTerm(a), unsafeReplaceTerm(b))
    case :&(a, b) => :&(unsafeReplaceTerm(a), unsafeReplaceTerm(b))
    case :!(a) => :!(unsafeReplaceTerm(a))
    case Predicate(name, args) => Predicate(name, args.map(unsafeReplaceTerm(_).asInstanceOf[Term]))
    case q@FA(name, expr) => if (name == replacement._1) q else FA(name, unsafeReplaceTerm(expr))
    case q@EX(name, expr) => if (name == replacement._1) q else EX(name, unsafeReplaceTerm(expr))
    case t@Term(name, Nil) => if (name == replacement._1.content) replacement._2 else t
    case Term(name, args) => Term(name, args.map(unsafeReplaceTerm(_).asInstanceOf[Term]))
    case x => x //???
  }

  private def rewrite(from: Term, to: Term)(in: Expr) = unsafeReplaceTerm(in)((from, to))

  val Zero = Term("0", Seq.empty)

  lazy val baseRev = parseUnsafe(Seq(
    "((a=b)->(b=a))->(A&B->B)->(a=b)->(b=a)"
    , "(A&B->B)->(a=b)->(b=a)"
    , "(A&B->B)->@b((a=b)->(b=a))"
    , "(A&B->B)->@a@b((a=b)->(b=a))"
    , "@a@b((a=b)->(b=a))")).reverse

  lazy val baseABAB2 = parseUnsafe(Seq(
    "(a+b'=(a+b)')"
    , "(a+b'=(a+b)')->(A&B->B)->(a+b'=(a+b)')"
    , "(A&B->B)->(a+b'=(a+b)')"
    , "(A&B->B)->@b(a+b'=(a+b)')"
    , "(A&B->B)->@a@b(a+b'=(a+b)')"
    , "@a@b(a+b'=(a+b)')")).reverse

  lazy val baseABAB = parseUnsafe(Seq(
    "(a=b)->(a'=b')"
    , "((a=b)->(a'=b'))->(A&B->B)->(a=b)->(a'=b')"
    , "(A&B->B)->(a=b)->(a'=b')"
    , "(A&B->B)->@b((a=b)->(a'=b'))"
    , "(A&B->B)->@a@b((a=b)->(a'=b'))"
    , "@a@b((a=b)->(a'=b'))")).reverse

  lazy val baseA0A = parseUnsafe(Seq("(a+0=a)->(A&B->B)->(a+0=a)"
    , "(A&B->B)->(a+0=a)"
    , "(A&B->B)->@a(a+0=a)"
    , "@a(a+0=a)")).reverse

  def genA0A(a: Term) = Seq((a :+ Zero) := a, FA("a", (Term("a") :+ Zero) := "a") ->: ((a :+ Zero) := a)) ++ baseA0A

  def genABAB(a: Term, b: Term): Seq[Expr] = {
    parseUnsafe(Seq(
      "@a@b((a=b)->(a'=b'))->@b((a=b)->(a'=b'))"
      , "@b((a=b)->(a'=b'))"
      , "@b((a=b)->(a'=b'))->((a=b)->(a'=b'))"
      , "(a=b)->(a'=b')"
      , "a'=b'"))
      .reverse
      .map(rewrite("a", a))
      .map(rewrite("b", b))
      .++(baseABAB)
  }

  def genAB2AB2(a: Term, b: Term): Seq[Expr] = {
    parseUnsafe(Seq(
      "@a@b(a+b'=(a+b)')->@b(a+b'=(a+b)')"
      , "@b(a+b'=(a+b)')"
      , "@b(a+b'=(a+b)')->(a+b'=(a+b)')"
      , "(a+b'=(a+b)')"))
      .reverse
      .map(rewrite("a", a))
      .map(rewrite("b", b))
      .++(baseABAB2)
  }

  def genRev(a: Term, b: Term): Seq[Expr] = {
    parseUnsafe(Seq(
      "@a@b((a=b)->(b=a))->@b((a=b)->(b=a))"
      , "@b((a=b)->(b=a))"
      , "@b((a=b)->(b=a))->(a=b)->(b=a)"
      , "(a=b)->(b=a)"
      , "(b=a)"))
      .reverse
      .map(rewrite("a", a))
      .map(rewrite("b", b))
      .++(baseRev)
  }

  def finishProof(a: Term, b: Term, c: Term): Seq[Expr] = {
    parseUnsafe(Seq(
      "@a@b@c((a=b)->(a=c)->(b=c))->@b@c((a=b)->(a=c)->(b=c))"
      , "@b@c((a=b)->(a=c)->(b=c))"
      , "@b@c((a=b)->(a=c)->(b=c))->@c((a=b)->(a=c)->(b=c))"
      , "@c((a=b)->(a=c)->(b=c))"
      , "@c((a=b)->(a=c)->(b=c))->((a=b)->(a=c)->(b=c))"
      , "(a=b)->(a=c)->(b=c)"
      , "(a=c)->(b=c)"
      , "b=c"))
      .reverse
      .map(rewrite("a", a))
      .map(rewrite("b", b))
      .map(rewrite("c", c))
      .++(baseRev)
  }

  lazy val abEQba = parseUnsafe(Seq(
    "a=b->a=c->b=c"
    , "A&B->B"
    , "(a=b->a=c->b=c)->((A&B->B)->(a=b->a=c->b=c))"
    , "(A&B->B)->(a=b->a=c->b=c)"
    , "(A&B->B)->@c(a=b->a=c->b=c)"
    , "@c(a=b->a=c->b=c)"
    , "@c(a=b->a=c->b=c)->(a=b->a=a->b=a)"
    , "a=b->a=a->b=a"
    , "a=a->a=b->a=a"
    , "(a=b->a=a->b=a)->a=a->a=b->a=a->b=a"
    , "a=a->a=b->a=a->b=a"
    , "(a=b->a=a)->(a=b->a=a->b=a)->a=b->b=a"
    , "((a=b->a=a)->(a=b->a=a->b=a)->a=b->b=a)->a=a->(a=b->a=a)->(a=b->a=a->b=a)->a=b->b=a"
    , "a=a->(a=b->a=a)->(a=b->a=a->b=a)->a=b->b=a"
    , "(a=a->a=b->a=a)->(a=a->(a=b->a=a)->(a=b->a=a->b=a)->a=b->b=a)->a=a->(a=b->a=a->b=a)->a=b->b=a"
    , "(a=a->(a=b->a=a)->(a=b->a=a->b=a)->a=b->b=a)->a=a->(a=b->a=a->b=a)->a=b->b=a"
    , "a=a->(a=b->a=a->b=a)->(a=b->b=a)"
    , "(a=a->a=b->a=a->b=a)->(a=a->(a=b->a=a->b=a)->a=b->b=a)->a=a->a=b->b=a"
    , "(a=a->(a=b->a=a->b=a)->a=b->b=a)->a=a->a=b->b=a"
    , "a=a->a=b->b=a"
    , "(A&B->B)->@b@c(a=b->a=c->b=c)"
    , "(A&B->B)->@a@b@c(a=b->a=c->b=c)"
    , "@a@b@c(a=b->a=c->b=c)"
    , "@a@b@c(a=b->a=c->b=c)->@b@c(a+0=b->a+0=c->b=c)"
    , "@b@c(a+0=b->a+0=c->b=c)"
    , "@b@c(a+0=b->a+0=c->b=c)->@c(a+0=a->a+0=c->a=c)"
    , "@c(a+0=a->a+0=c->a=c)"
    , "@c(a+0=a->a+0=c->a=c)->(a+0=a->a+0=a->a=a)"
    , "a+0=a"
    , "(a+0=a->a+0=a->a=a)"
    , "a+0=a->a=a"
    , "a=a"
    , "a=b->b=a")).reverse

}
