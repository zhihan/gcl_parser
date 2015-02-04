package me.zhihan.gcl

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
sealed abstract class Operand

case class Disjunction(
  val clauses: List[Conjunction]) extends Operand {}


case class Conjunction(val clauses: List[Comparison]) {}

sealed abstract class Comparison
case class SimpleComp(val s: Sum) extends Comparison {}
case class Comp(val op: Types.RelOp,
  val lhs: Sum, val rhs: Sum) extends Comparison {}

sealed abstract class Sum
case class SimpleSum(val term:Term) extends Sum {}
case class BinarySum(val op:Types.AdditiveOp,
  val lhs: Term, val rhs: Sum) extends Sum {}

sealed abstract class Term
case class SimpleTerm(val factor: Factor) extends Term
case class BinaryTerm(val op:Types.MultiplicativeOp,
  val lhs: Factor,
  val rhs: Term) extends Term {}

case class Factor(val operand: Operand,
  val op: Option[Types.UnaryOp],
  val modifier: Option[Structure]) {}

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
}

case class ListExpression(val value:List[Types.Expression]) extends Operand {}

/** Value is synonymous to assignment */
case class Value(val value: Operand) {}

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
