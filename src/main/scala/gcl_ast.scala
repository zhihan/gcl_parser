package me.zhihan.gcl

import scala.collection.mutable.Map

/**
  *  Abstract Syntax Tree for GCL
  * 
  *  AST serves as an intermediate representation between the parser
  *  and the interpreter.
  */

case class TypeError(message:String) extends Exception(message) {}

/** A list of types aliases used in the AST. */
object Types {
  type Identifier = String 
  type FieldProperties = List[String]
  type FileName = String
  type Expression = Disjunction
  type RelOp = String
  type AdditiveOp = String
  type MultiplicativeOp = String
  type UnaryOp = String
}

/** Expression */
sealed abstract class Exp

/** A disjunction of logical expressions. */
case class Disjunction(
  val clauses: List[Exp]) extends Exp {}

/** A conjunction of logical expressions. */
case class Conjunction(
  val clauses: List[Exp]) extends Exp {}

/** A degenerate case of comparison. TODO: investigate whether this is needed. */
case class SimpleComp(val s: Exp) extends Exp {}

/** Comparison */
case class Comp(val op: Types.RelOp,
  val lhs: Exp, val rhs: Exp) extends Exp {}

/** A sum of one or more expressions. */
case class Sum(
  val lhs: Exp,
  val tail: List[(Types.AdditiveOp, Exp)]) extends Exp {}

/** Multiplication of one or more expressions. */
case class Term(
  val lhs: Exp,
  val tail: List[(Types.MultiplicativeOp, Exp)]) extends Exp {}

/** An atomic expression that can be used for multiplications. */
case class Factor(val operand: Exp, 
  val op: Option[Types.UnaryOp],
  val modifier: Option[Structure]) extends Exp {}

/** Integer literal. */
case class IntegerLiteral(val i:Int) extends Exp {}

/** String literal. */
case class StringLiteral(val s:String) extends Exp {}

/** Boolean literal. */
case class BooleanLiteral(val b:Boolean) extends Exp {}

/** References */
sealed abstract class Reference extends Exp
case class SuperReference(
  val path: List[Types.Identifier]) extends Reference {}

/** Relative references */
case class RelativeReference(
  val path:List[Types.Identifier]) extends Reference {}

/** Up references */
case class UpReference(val ref:Reference) extends Reference {}

/** Absolute references */
case class AbsoluteReference(
  val path:List[Types.Identifier]) extends Reference {}

/** Null is a special expression */
case object Null extends Exp {} 

object Operand {
  def isTrue(o:Exp) = o == BooleanLiteral(true)
  def isFalse(o:Exp) = o == BooleanLiteral(false)
  def isString(o:Exp, s:String) = o == StringLiteral(s)
  def isInt(o:Exp, i:Int) = o == IntegerLiteral(i)
}


object Exp {
  def simplify(e: Exp): Exp = {
    e match {
      case Disjunction(List(x)) => simplify(x)
      case Conjunction(List(x)) => simplify(x)
      case SimpleComp(sc) => simplify(sc)
      case Sum(x, List()) => simplify(x)
      case Term(x, List()) => simplify(x)
      case Factor(x, None, None) => simplify(x)
      case _ => e
          
    }
  }
}

case class ListExpression(
  val value:List[Types.Expression]) extends Exp {}

/** Value is synonymous to assignment */
case class Value(val value: Exp) {}

/* Field definitions */
sealed abstract class FieldProperty 
case object Final extends FieldProperty {}
case object Local extends FieldProperty {}
case object Template extends FieldProperty {}
case object ValidationIgore extends FieldProperty {}

case class ExpansionInvocation() {
}

case class FieldHeader(
  val props: Types.FieldProperties,
  val t: Option[Types.Identifier], 
  val id: Types.Identifier) {}

case class Structure(val entries: List[Entry]) extends Exp {} 

/** 
  * Entry 
  * 
  * An entry is similar to a statement in an imperative language.
  */
sealed abstract class Entry

/** Import statement */
case class Import(
  val fileName: Types.FileName,
  val as: Types.Identifier) extends Entry {}

/** Field definition */
case class Field(
  val header: FieldHeader,
  val value: Value) extends Entry {}

case class Check() extends Entry {}
case class Expansion() extends Entry {}

/** 
  *  Resolution step
  * In the resolution step, the references for the fields are resolved.
  * to an AST element.
  */


case class Scope(
  val parent: Option[Scope],
  val sup:Option[Scope],
  val scope: Map[String, Exp]) extends Exp {

  override def toString = "Scope " + (if (parent.isEmpty) "" else "$")

  def copy = Scope(parent, sup, scope.clone)

  private def resolveLocal(id:String): Option[Exp] = 
    scope.get(id)

  private def resolveLocal(id: List[String]): Option[Exp] = {
    id match {
      case List(h) => resolveLocal(h)
      case h :: tl => resolveLocal(h).flatMap{
        case x:Scope => x.resolveNoParent(tl)
        case _ => None
      }
    }
  }

  def resolveIn(s: Option[Scope], id:String) =
    s.flatMap{ sc => sc.resolve(id) }

  def resolveIn(s: Option[Scope], id:List[String]) =
    s.flatMap{ sc => sc.resolve(id) }

  def resolve(id: String): Option[Exp] = {
    resolveLocal(id) orElse resolveIn(sup, id) orElse resolveIn(parent, id)
  }

  def resolveNoParent(id:List[String]): Option[Exp] = {
    resolveLocal(id) orElse resolveIn(sup, id)
  }
  def resolve(id: List[String]): Option[Exp] = {
    resolveLocal(id) orElse resolveIn(sup, id) orElse resolveIn(parent, id)
  }

  def resolve(ref:Reference): Option[Exp] = {
    ref match {
      case SuperReference(id) => ???
      case RelativeReference(id) => resolve(id)
      case UpReference(id) => ???
      case AbsoluteReference(id) => ???
    }
  }

  def inlineModifiers {
    val keys = scope.keySet
    keys.foreach { id =>
      scope(id) match {
        case Factor(r, None, Some(modifier)) => {
          if (!r.isInstanceOf[Reference]) {
            throw TypeError("Cannot modify a non-referenced value")
          }
          val ref = r.asInstanceOf[Reference]
          val proto = resolve(ref).get
          if (!proto.isInstanceOf[Scope]) {
            throw TypeError("Trying to modify a non-structure value")
          }
          val rhs = Scope.newModifiedScope(proto.asInstanceOf[Scope],
            modifier, this)

          scope(id) = rhs
          rhs.inlineModifiers
        }
        case _ => ()
      }
    }
  }

}

object Scope {
  /** Creating a new scope object from a structure AST */
  def newScope(struct: Structure, 
    parent: Scope = null, 
    sup: Scope = null): Scope = {
    val p = if (parent != null) Some(parent) else None
    val s = if (sup != null) Some(sup) else None

    val m = Map[String, Exp]()

    struct.entries.foreach{
      case Import(_,_) => ()
      case Field(FieldHeader(_,_,id), Value(v)) => {
        m += (id -> Exp.simplify(v))
      }
      case Check() => ()
      case Expansion() => ()
    }

    // Create scope for structures
    val self = Scope(p, s, m)
    m.foreach { case (id: String, v: Exp) =>
      v match {
        case s:Structure => m(id) = newScope(s, self)
        case _ => ()
      }
      case _ => ()
    }
    self
  }

  /** Create a modified scope object from a structure AST */
  def newModifiedScope(prototype: Scope,
    modifier: Structure,
    parent: Scope = null): Scope =
    newScope(modifier, parent, prototype)


}
 
