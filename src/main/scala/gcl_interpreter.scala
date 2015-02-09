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


class Interpreter(val ctx:StructVal) {
  def evalDisjunction(l:List[Exp]) : Val = {
    if (l.size == 1) {
      evalExp(l(0))
    } else {
      l map { evalExp } reduce {
        (x:Val, y:Val) =>
        (x,y) match {
          case (BoolVal(a), BoolVal(b)) => BoolVal(a || b)
          case _ => throw TypeError("Expecting Boolean value in disjunction")
        }
      }
    }
  }

  def evalConjunction(l:List[Exp]) : Val = {
    if (l.size == 1) {
      evalExp(l(0))
    } else {
      l map { evalExp(_)} reduce { (x:Val, y: Val) =>
        (x, y) match {
          case (BoolVal(a), BoolVal(b)) => BoolVal(a && b)
          case _ => throw TypeError("Expecting Boolean value in conjunction")
        }
      }
    }
  }

  def evalComp(op:String, lval:Val, rval:Val) : Val = {
    (op, lval, rval) match {
      case ("<", IntVal(l), IntVal(r)) => BoolVal(l < r)
      case (">", IntVal(l), IntVal(r)) => BoolVal(l > r)
      case ("<=", IntVal(l), IntVal(r)) => BoolVal(l <= r)
      case (">=", IntVal(l), IntVal(r)) => BoolVal(l >= r)
      case ("==", IntVal(l), IntVal(r)) => BoolVal(l == r)
      case ("!=", IntVal(l), IntVal(r)) => BoolVal(l != r)

      case ("==", StringVal(l), StringVal(r)) => BoolVal(l == r)
      case ("!=", StringVal(l), StringVal(r)) => BoolVal(l != r)

      case ("==", BoolVal(l), BoolVal(r)) => BoolVal(l == r)
      case ("!=", BoolVal(l), BoolVal(r)) => BoolVal(l != r)

      case _ => throw TypeError("Unsupported comparison")
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

    s.tail.foldLeft (evalExp(s.lhs)) { (l, r) =>
      r match {
        case (op, f) => evalOp(l, op, evalExp(f))
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

    t.tail.foldLeft (evalExp(t.lhs)) { (l, r) =>
      r match {
        case (op, f) => evalOp(l, op, evalExp(f))
      }
    }
  }

  def evalFactor(f: Factor) = {
    evalExp(f.operand)
  }

  def evalReference(ref: Reference) = {
    NullVal;
  }

  def evalField(field: Field) {
    val rhs = evalExp(field.value.value)
    val id = field.header.id
    ctx.assignIn(id, rhs)
  }

  def evalExp(exp:Exp) : Val = {
    exp match {
      case IntegerLiteral(i) => IntVal(i)
      case BooleanLiteral(b) => BoolVal(b)
      case StringLiteral(s) => StringVal(s)
      case ref:Reference => evalReference(ref)
      case ListExpression(l) => ???
      case Null => NullVal
      case Structure(entries) => ???
      case Scope(_,_,_) => ???

      case s:Sum => evalSum(s)
      case t:Term => evalTerm(t)
      case f:Factor => evalFactor(f)

      case SimpleComp(sc) => evalExp(sc)
      case Comp(op, l, r) => evalComp(op,
        evalExp(l), evalExp(r))
      case Disjunction(cl) => evalDisjunction(cl)
      case Conjunction(cl) => evalConjunction(cl)
        
    }
  }

}
