package me.zhihan.gcl

import scala.collection.mutable.Map

import org.scalatest.FunSuite
import org.scalactic._
import TripleEquals._

class GCLInterpreterTests extends FunSuite {
  def parseStructure(x: String) =
    GCLParser.parseAll(GCLParser.structure, x).get

  test("Scope copy") {
    val a = Scope(None, None, Map("a" -> IntegerLiteral(1)))
    val b = a.copy
    b.scope += ("b" -> IntegerLiteral(2))
    assert(b.scope.size === 2)
    assert(a.scope.size === 1)
  }

  test("Resolution in scopes") {
    val struct = parseStructure("""{ a = 1}""")
    val scope1 = Scope.newScope(struct)
    assert(!scope1.resolve("a").isEmpty)
    assert(scope1.resolve("b").isEmpty)

    val struct2 = parseStructure("""{ b = 1}""")
    val scope = Scope.newScope(struct2, parent = scope1)
    assert(!scope.resolve("a").isEmpty)
    assert(!scope.resolve("b").isEmpty)
  }

  test("Resolution full path") {
    val struct = parseStructure("""{ a = { b = 1 } } """)
    val scope = Scope.newScope(struct)
    assert(!scope.resolve(List("a", "b")).isEmpty)
    assert(scope.resolve(List("a", "a")).isEmpty)
  }

  test("Resolution with modifiers") {
    val struct = parseStructure("""
{ T = { b = 1 },
  a = T { c = 2 } 
} """)
    val scope = Scope.newScope(struct)
    scope.flattenAllModifiers
    assert(scope.resolve(List("a", "b")).get ===
      IntegerLiteral(1))
    assert(scope.resolve(List("a", "c")).get ===
      IntegerLiteral(2))
  }

  def evalLit(x:String) = {
    val ctx = StructVal()
    new Interpreter(ctx).evalOperand(
      GCLParser.parseAll(GCLParser.literal, x).get)
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

  test("Interpret field") {
    val ctx = StructVal()
    val field = GCLParser.parseAll(GCLParser.field, "x = 1").get
    new Interpreter(ctx).evalField(field)
    val actual = ctx.evalIn("x")
    assert(actual === IntVal(1))
  }

  def evalExp(x:String) = {
    val ctx = StructVal()
    new Interpreter(ctx).evalOperand(
      GCLParser.parseAll(GCLParser.expression, x).get)
  }

  test("Interpret expressions") {
    assert(evalExp("1+2") === IntVal(3))
    assert(evalExp("1+2-1") === IntVal(2))
    assert(evalExp("4/2*4") === IntVal(8))
    assert(evalExp("4+2*4") === IntVal(12))
    assert(evalExp("4/2+4") === IntVal(6))

    assert(evalExp("2>1") === BoolVal(true))
    assert(evalExp("2<1") === BoolVal(false))
    assert(evalExp("2<=1") === BoolVal(false))
    assert(evalExp("2>=1") === BoolVal(true))
    assert(evalExp("2==1") === BoolVal(false))
    assert(evalExp("2!=1") === BoolVal(true))

    assert(evalExp("'a'!='b'") === BoolVal(true))
    assert(evalExp("'a'=='b'") === BoolVal(false))
  }

}
