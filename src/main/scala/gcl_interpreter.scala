package me.zhihan.gcl

/** Value classes */

sealed abstract class Val 

case class IntVal(val i: Integer) extends Val

case class BoolVal(val b: Boolean) extends Val

case class StringVal(val s: String) extends Val

case object NullVal extends Val {}

object Interpreter {
  def evalOperand(o: Operand) = {
    o match {
      case IntegerLiteral(i) => IntVal(i)
      case BooleanLiteral(b) => BoolVal(b)
      case StringLiteral(s) => StringVal(s)
      case _ => NullVal
    }
  }

}
