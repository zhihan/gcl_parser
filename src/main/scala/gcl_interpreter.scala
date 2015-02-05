package me.zhihan.gcl

import scala.collection.mutable.Map

case class ReassignException(message:String) extends Exception(message) {}

/** Value classes */

sealed abstract class Val 

case class IntVal(val i: Integer) extends Val

case class BoolVal(val b: Boolean) extends Val

case class StringVal(val s: String) extends Val

case object NullVal extends Val {}

/** Structs */
case class StructVal(
  val parent: Option[StructVal] = None,
  val sup: Option[StructVal] = None,
  val scope:Map[String, Val] = Map[String, Val](),
  val t: Option[String] = None) {

  def assignIn(id: String, value: Val) {
    if (scope contains id) {
      throw ReassignException("reassign the same value")
    }
    scope(id) = value
  }

  /** Evaluate the variable within this scope */
  def evalIn(id: String) = {
    scope.getOrElse(id, NullVal)
  }
}

case class TypeError(message:String) extends Exception(message) {}

class Interpreter(val ctx:StructVal) {
  def evalDisjunction(l:List[Conjunction]) : Val = {
    if (l.size == 1) {
      evalConjunction(l(0).clauses)
    } else {
      l map { (x:Conjunction) => evalConjunction(x.clauses)} reduce {
        (x:Val, y:Val) =>
        (x,y) match {
          case (BoolVal(a), BoolVal(b)) => BoolVal(a || b)
          case _ => throw TypeError("Expecting Boolean value in disjunction")
        }
      }
    }
  }

  def evalConjunction(l:List[Comparison]) : Val = {
    if (l.size == 1) {
      evalComparison(l(0))
    } else {
      l map { evalComparison(_)} reduce { (x:Val, y: Val) =>
        (x, y) match {
          case (BoolVal(a), BoolVal(b)) => BoolVal(a && b)
          case _ => throw TypeError("Expecting Boolean value in conjunction")
        }
      }
    }
  }

  def evalComparison(c: Comparison) : Val = {
    c match {
      case SimpleComp(s) => evalSum(s)
      case Comp(_,_,_) => ???
    }
  }

  def evalSum(s: Sum) : Val = {
    def evalOp(l:Val, op:String, r:Val) = {
      (l, op, r) match {
        case (IntVal(x), "+", IntVal(y)) => IntVal(x + y)
        case (IntVal(x), "-", IntVal(y)) => IntVal(x - y)
        case _ => throw TypeError("Type error in sum")
      }
    }

    s.tail.foldLeft (evalTerm(s.lhs)) { (l, r) =>
      r match {
        case (op, f) => evalOp(l, op, evalTerm(f))
      }
    }
  }
  

  def evalTerm(t: Term) = {
    def evalOp(l:Val, op:String, r:Val) = {
      (l, op, r) match {
        case (IntVal(x), "*", IntVal(y)) => IntVal(x * y)
        case (IntVal(x), "/", IntVal(y)) => IntVal(x / y)
        case _ => throw TypeError("Type error in multiply")
      }
    }

    t.tail.foldLeft (evalFactor(t.lhs)) { (l, r) =>
      r match {
        case (op, f) => evalOp(l, op, evalFactor(f))
      }
    }
  }

  def evalFactor(f: Factor) = {
    evalOperand(f.operand)
  }


  def evalOperand(o: Operand) = {
    o match {
      case IntegerLiteral(i) => IntVal(i)
      case BooleanLiteral(b) => BoolVal(b)
      case StringLiteral(s) => StringVal(s)
      case Disjunction(clauses) => evalDisjunction(clauses)
      case _ => NullVal
    }
  }

  def evalField(field: Field) {
    val rhs = evalOperand(field.value.value)
    val id = field.header.id
    ctx.assignIn(id, rhs)
  }

}
