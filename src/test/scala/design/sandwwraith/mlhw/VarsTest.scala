package design.sandwwraith.mlhw

import design.sandwwraith.mlhw.model.Term
import org.scalatest.{FunSuite, Matchers}

class VarsTest extends FunSuite with Matchers {
  test("substitution") {
    val a = new Term("a")
    val x = new Term("x")
    //substitute x for a
    val free = List("@xP(x)",
      "?x(x=x)",
      "?xP(x)->x",
      "?a(P(w)->P(a))",
      "x->x",
      "@xP(x)",
      "?x((x*0''')=(0'''*x))",
      "Q(a,b,x)->@x@a(Q(x)->a)").map(new ExpressionParser(_).inputLine.run().get)
    assert(free.forall(p => p.isFreeForSubstitution(x, a)))
    val notFree = List("?a(P(x)->P(a))",
      "?x(Q(x))->@a(P(a)->Q(x))",
      "@aP(a,b,c,x)").map(new ExpressionParser(_).inputLine.run().get)
    assert(notFree.forall(p => !p.isFreeForSubstitution(x, a)))
  }

  test("enters free") {
    val x = new Term("x")
    val yes = List("x",
      "P(a,b,c,d)->Q(x,x)",
      "?xP(x)->x",
      "@a(P(a)->x)").map(new ExpressionParser(_).inputLine.run().get)
    assert(yes.forall(p => p.varEntersFree(x)))
    val no = List("?x(x=0'')",
      "?dP(a,b,c,d)->?xQ(x,x)",
      "?x(P(x)->x)",
      "@a@x(P(a)->x)").map(new ExpressionParser(_).inputLine.run().get)
    assert(no.forall(p => !p.varEntersFree(x)))
  }

  test("is substituted") {
    val a = new Term("a")
    val x = new Term("x")
    val yes = List("x" -> "t+0",
      "x" -> "q",
      "(x=t->x=t->t=t)" -> "(t+0=t->t+0=t->t=t)",
      "(x=x)" -> "(z=z)",
      "P(x)" -> "P(a)",
      "Q(x,y,z)" -> "Q(a,y,z)",
//      "P(x,x)->?xQ(x)" -> "P(a,a)->?xQ(x)",
      "P(x,a)" -> "P(a,a)",
      "x&x"->"x&x",
      "x" -> "q->q"
    ).map(p => (new ExpressionParser(p._1).inputLine.run().get, new ExpressionParser(p._2).inputLine.run().get))
    assert(yes.forall(p => p._1.isSubstituted(x, p._2)))
    val no = List("(x=x)" -> "(z=a)",
      "@z(P(x,y)->P(z))" -> "@z(P(x,z)->P(z))",
      "Q(x,y,z)" -> "Q(y,x,z)",
      "P(x,x)->?xQ(x)" -> "P(a,a)->?yQ(y)",
      "P(x,a)" -> "Q(a,a)",
      "x&a"->"x&x",
      "P(x)" -> "Q(a)"
    ).map(p => (new ExpressionParser(p._1).inputLine.run().get, new ExpressionParser(p._2).inputLine.run().get))
    assert(no.forall(p => !p._1.isSubstituted(x, p._2)))
  }
}
