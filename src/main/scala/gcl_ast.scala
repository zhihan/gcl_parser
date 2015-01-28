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

/* Field definitions */
abstract class FieldProperty 
case object Final extends FieldProperty {}
case object Local extends FieldProperty {}
case object Template extends FieldProperty {}
case object ValidationIgore extends FieldProperty {}


case class Field(
  val fieldHeader: FieldHeader,
  val value: FieldValue,
  val expansionInvocation: Option[ExpansionInvocation]
) {}

case class FieldValue() {
}

case class ExpansionInvocation() {
}

case class StringLiteral(val value:String) {}

case class FieldHeader(
  val props: Types.FieldProperties,
  val id: Types.Identifier) {}

/** Import statement */
case class Import(
  val fileName: Types.FileName,
  val as: Types.Identifier) {}



/** Expression */
case class Disjunction(val clauses: List[Conjunction]) {}
case class Conjunction(val clauses: List[Comparison]) {}

abstract class Comparison
case class SimpleComp(val s: Sum) extends Comparison {}
case class Comp(val op: Types.RelOp,
  val lhs: Sum, val rhs: Sum) extends Comparison {}

abstract class Sum
case class SimpleSum(val term:Term) extends Sum {}
case class BinarySum(val op:Types.AdditiveOp,
  val lhs: Term, val rhs: Sum) extends Sum {}

abstract class Term
case class SimpleTerm(val factor: Factor) extends Term
case class BinaryTerm(val op:Types.MultiplicativeOp,
  val lhs: Factor,
  val rhs: Term) extends Term {}

case class Factor(val operand: Operand,
  val op: Option[Types.UnaryOp],
  val modifier: Option[Structure]) {}

case class Structure() {} 

abstract class Operand
case class IntegerLiteral(val i:Int) extends Operand  
