package me.zhihan.gcl

import org.scalatest.FunSuite
import org.scalactic._
import TripleEquals._

class GCLInterpreterTests extends FunSuite {
  def evalLit(x:String) = {
    Interpreter.evalOperand(GCLParser.parseAll(GCLParser.literal, x).get)
  }

  test("Interpret literals") {
    assert(evalLit("1") === IntVal(1))
    assert(evalLit("10") === IntVal(10))
    assert(evalLit("0x10") === IntVal(16))
    assert(evalLit("10K") === IntVal(10240))

    assert(evalLit("true") === BoolVal(true))
    assert(evalLit("false") === BoolVal(false))

    assert(evalLit("'a'") === StringVal("a"))
    assert(evalLit("\"a\"") === StringVal("a"))
  }

}
