package me.zhihan.gcl

import scala.collection.mutable.Map

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
// The root expression with least precedence is the disjunction of
// logical expressions.
sealed abstract class Exp
sealed abstract class Operand extends Exp 

case class Disjunction(
  val clauses: List[Exp]) extends Exp {}

case class Conjunction(
  val clauses: List[Exp]) extends Exp {}

case class SimpleComp(val s: Exp) extends Exp {}
case class Comp(val op: Types.RelOp,
  val lhs: Exp, val rhs: Exp) extends Exp {}

case class Sum(
  val lhs: Exp,
  val tail: List[(Types.AdditiveOp, Exp)]) extends Exp {}

case class Term(
  val lhs: Exp,
  val tail: List[(Types.MultiplicativeOp, Exp)]) extends Exp {}

case class Factor(val operand: Exp, 
  val op: Option[Types.UnaryOp],
  val modifier: Option[Structure]) extends Exp {}

case class IntegerLiteral(val i:Int) extends Operand {}
case class StringLiteral(val s:String) extends Operand {}
case class BooleanLiteral(val b:Boolean) extends Operand {}

sealed abstract class Reference extends Operand
case class SuperReference(
  val path: List[Types.Identifier]) extends Reference {}

case class RelativeReference(
  val path:List[Types.Identifier]) extends Reference {}

case class UpReference(val ref:Reference) extends Reference {}

case class AbsoluteReference(
  val path:List[Types.Identifier]) extends Reference {}

case object Null extends Operand {} 

object Operand {
  def isTrue(o:Operand) = o == BooleanLiteral(true)
  def isFalse(o:Operand) = o == BooleanLiteral(false)
  def isString(o:Operand, s:String) = o == StringLiteral(s)
  def isInt(o:Operand, i:Int) = o == IntegerLiteral(i)

  def flatten(o:Exp) : Exp = { o
  }
}

case class ListExpression(val value:List[Types.Expression]) extends Operand {}

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

case class Structure(val entries: List[Entry]) extends Operand {} 

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

  def flattenModifier(o:Exp) = o

  def flattenAllModifiers {
    val keys = scope.keySet
    keys.foreach { id =>
      val rhs = flattenModifier(scope(id))
      scope(id) = rhs
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
        m += (id -> Operand.flatten(v))
      }
      case Check() => ()
      case Expansion() => ()
    }

    // Create scope for structures
    val self = Scope(p, s, m)
    m.foreach { case (id: String, v: Operand) =>
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
 
