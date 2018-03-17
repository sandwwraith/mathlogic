package design.sandwwraith.mlhw.model

import design.sandwwraith.mlhw.model.ExprTypes.EvalContext

abstract sealed class Expr(val symbol: String) {
  def ->:(other: Expr) = ->(other, this)
  def V(other: Expr): Expr = :|(this, other)

  def &(other: Expr): Expr = :&(this, other)

  def !!(other: Expr): Expr = :!(this)

  val vars: List[String]
  def terms: Seq[Term]

  def eval(implicit ctx: EvalContext): Boolean

  private val commonPredicates = Seq("=", "*", "+")

  protected def stringifyExprWithArgs(args: Seq[Term]): String = {
    val name = symbol
    if (args.lengthCompare(2) == 0 && commonPredicates.contains(name))
      args(0) + " " + name + " " + args(1)
    else if (name == "'") {
      (args(0) match {
        case Term(_, Nil) => args(0)
        case Term(_, _) => "(" + args(0) + ")"
      }) + "'"
    }
    else if (args.isEmpty) name
    else name + "(" + args.mkString(",") + ")"
  }

  def isFreeForSubstitution(x: Term, phi: Expr, affectedVars: Set[Term] = Set()): Boolean = this match {
    case Quantifier(v, expr) => v == x || expr.isFreeForSubstitution(x, phi, affectedVars + v)
    case t@Term(name, Nil) if t == x => phi.terms.forall(p => !affectedVars.contains(p))
    case Term(name, args) => args.forall((p:Term) => p != x || phi.terms.forall(p => !affectedVars.contains(p)))
    case Predicate(name, args) => args.forall((p:Term) => p != x || phi.terms.forall(p => !affectedVars.contains(p)))
    case BinaryExpr(a, b) => a.isFreeForSubstitution(x, phi, affectedVars) && b.isFreeForSubstitution(x, phi, affectedVars)
    case :!(a) => a.isFreeForSubstitution(x, phi, affectedVars)
    case _ => true
  }

  def varEntersFree(phi: Term): Boolean = this match {
    case Quantifier(v, expr) if v == phi => false
    case Quantifier(v, expr) => expr.varEntersFree(phi)
    case t@Term(name, Nil) => t == phi
    case Term(name, args) => args.exists(p => p.varEntersFree(phi))
    case Predicate(name, args) => args.exists(p => p.varEntersFree(phi))
    case BinaryExpr(a, b) => a.varEntersFree(phi) || b.varEntersFree(phi)
    case :!(a) => a.varEntersFree(phi)
    case _ => true
  }

  def substitute(variables: Map[String, Expr]): Expr = this match {
    case FA(x, b) => FA(x, b.substitute(variables))
    case EX(x, b) => EX(x, b.substitute(variables))
    case t@Term(name, Nil) => variables.getOrElse(name, t)
    case ->(a, b) => ->(a.substitute(variables), b.substitute(variables))
    case :&(a, b) => :&(a.substitute(variables), b.substitute(variables))
    case :|(a, b) => :|(a.substitute(variables), b.substitute(variables))
    case :!(a) => :!(a.substitute(variables))
    case Predicate(name, b) => Predicate(name, b.map(_.substitute(variables).asInstanceOf[Term]))
    case Term(name, b) => Term(name, b.map(_.substitute(variables).asInstanceOf[Term]))
    case e => e
  }

  def concat(a: Option[Set[Expr]], b: Option[Set[Expr]]) : Option[Set[Expr]] = (a,b) match {
    case (Some(l1), Some(l2)) => Some(l1 ++ l2)
    case _ => None
  }

  def findChanges(x: Term, other: Expr) :Option[Set[Expr]] = (this, other) match {
    case (FA(v1, e1), FA(v2, e2)) =>
      if (v1 != v2) None else e1.findChanges(x, e2)
    case (EX(v1, e1), EX(v2, e2)) =>
      if (v1 != v2) None else e1.findChanges(x, e2)
    case (t1@Term(n1, Nil), expr) if t1 != other =>
      Some(Set(expr))
    case (Term(n1, Nil), Term(n2, Nil)) =>
      Some(Set())
    case (Term(n1, args1), Term(n2, args2)) if n1 == n2 =>
      args1.zip(args2).map((p) => p._1.findChanges(x, p._2)).reduce[Option[Set[Expr]]]((a1,a2) => concat(a1,a2))
    case (Predicate(n1, args1), Predicate(n2, args2)) if n1 == n2 =>
      args1.zip(args2).map((p) => p._1.findChanges(x, p._2)).reduce[Option[Set[Expr]]]((a1,a2) => concat(a1,a2))
    case (->(a1, b1), ->(a2, b2)) =>
      concat(a1.findChanges(x, a2), b1.findChanges(x, b2))
    case (:&(a1, b1), :&(a2, b2)) =>
      concat(a1.findChanges(x, a2), b1.findChanges(x, b2))
    case (:|(a1, b1), :|(a2, b2)) =>
      concat(a1.findChanges(x, a2), b1.findChanges(x, b2))
    case (:!(a1), :!(a2)) =>
      a1.findChanges(x, a2)
    case _ => None

  }

  def isSubstituted(x: Term, other: Expr): Boolean = {
    val changes = findChanges(x, other)
    changes match {
      case None => false //not the same trees
      case Some(set) =>
        set.size match {
          case 0 => true
          case 1 =>
            isFreeForSubstitution(x, set.head) && other == substituteFree(x, set.head)
          case _ => false
        }
    }
  }

  def substituteFree(what:Term, to:Expr) : Expr = this match {
    case FA(x, b) if x == what => FA(x, b)
    case FA(x, b) => FA(x, b.substituteFree(what, to))
    case EX(x, b) if x == what => EX(x, b)
    case EX(x, b) => EX(x, b.substituteFree(what, to))
    case t@Term(_, Nil)  => if (t == what) to else t
    case Term(name, args) => Term(name, args.map(_.substituteFree(what, to).asInstanceOf[Term]))
    case Predicate(name, args) => Predicate(name, args.map(_.substituteFree(what, to).asInstanceOf[Term]))
    case ->(a, b) => ->(a.substituteFree(what, to), b.substituteFree(what, to))
    case :&(a, b) => :&(a.substituteFree(what, to), b.substituteFree(what, to))
    case :|(a, b) => :|(a.substituteFree(what, to), b.substituteFree(what, to))
    case :!(a) => :!(a.substituteFree(what, to))
  }
}

abstract sealed class BinaryExpr(val left: Expr, val right: Expr, symbol: String) extends Expr(symbol) {
  override def toString: String = s"($left $symbol $right)"

  override lazy val vars: List[String] = left.vars ++ right.vars
  override lazy val terms: Seq[Term] = left.terms ++ right.terms
}

object BinaryExpr {
  def unapply(arg: BinaryExpr): Option[(Expr, Expr)] = Some((arg.left, arg.right))
}

case class ->(private val a: Expr, private val b: Expr) extends BinaryExpr(a, b, "->") {
  def eval(implicit ctx: EvalContext): Boolean = !a.eval || b.eval
}

case class :|(private val a: Expr, private val b: Expr) extends BinaryExpr(a, b, "|") {
  def eval(implicit ctx: EvalContext): Boolean = a.eval || b.eval
}

case class :&(private val a: Expr, private val b: Expr) extends BinaryExpr(a, b, "&") {
  def eval(implicit ctx: EvalContext): Boolean = a.eval && b.eval
}

case class :!(expr: Expr) extends Expr("!") {
  override def toString: String = s"!$expr"

  override lazy val vars: List[String] = expr.vars
  override lazy val terms: Seq[Term] = expr.terms

  def eval(implicit ctx: EvalContext): Boolean = !expr.eval
}

abstract class Quantifier(val varName: Term, val expr: Expr, symbol: String) extends Expr(symbol) {
  def eval(implicit ctx: EvalContext): Boolean = expr.eval

  override lazy val vars: List[String] = expr.vars
  override lazy val terms: Seq[Term] = expr.terms

  override def toString: String = symbol + varName + "(" + expr + ")"
}

object Quantifier {
  def unapply(arg: Quantifier): Option[(Term, Expr)] = Some((arg.varName, arg.expr))
}

case class FA(override val varName: Term, override val expr: Expr) extends Quantifier(varName, expr, "@")

case class EX(override val varName: Term, override val expr: Expr) extends Quantifier(varName, expr, "?")

case class Const(v: Boolean) extends Expr(v.toString) {
  override val vars: List[String] = List.empty
  override lazy val terms: Seq[Term] = Seq.empty

  override def eval(implicit ctx: EvalContext): Boolean = v

  override def toString: String = v.toString
}

case class Predicate(name: String, args: Seq[Term]) extends Expr(name) {
  override val vars: List[String] = args.flatMap(_.vars).toList
  override lazy val terms: Seq[Term] = args.flatMap(_.terms)

  override def eval(implicit ctx: EvalContext): Boolean = throw new UnsupportedOperationException("Predicates can't be evaluated")

  override def toString: String = stringifyExprWithArgs(args)
}

case class Term(content: String, args: Seq[Term] = List.empty) extends Expr(content) {
  override lazy val vars: List[String] = List(content) ++ args.flatMap(_.vars).toList
  override lazy val terms: Seq[Term] = if (args.nonEmpty) args.flatMap(_.terms) else Seq(this)

  def eval(implicit ctx: EvalContext): Boolean = if (args.isEmpty) ctx(content) else throw new UnsupportedOperationException("Not a variable")

  override def toString: String = stringifyExprWithArgs(args)

  def :=(other: Term): Expr = Predicate("=", Seq(this, other))
  def :+(other: Term): Term = Term("+", Seq(this, other))
}


object ExprTypes {
  type Impl = ->
  type Disj = :|
  type Conj = :&
  type Neg = :!

  type EvalContext = Map[String, Boolean]

  implicit def toTerm(s: String) = Term(s)
}

