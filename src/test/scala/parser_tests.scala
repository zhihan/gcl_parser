package me.zhihan.gcl 

import org.scalatest.FunSuite

class GCLParserTests extends FunSuite {
  // Helper function to parse identifiers
  def parseIdent(x:String) =
    GCLParser.parseAll(GCLParser.identifier, x).successful

  test("Valid identifiers") {
    val validIds = List("a", "_a", "_", "_1", "a-b")
    assert(validIds.forall(parseIdent))
  }

  test("Invalid identifiers") {
    val invalidIds = List("0", "0a", "0_", "-a", "a b")
    assert(!invalidIds.exists(parseIdent))
  }

  def parseBoolean(x:String) = 
    GCLParser.parseAll(GCLParser.booleanLiteral, x)

  test("Boolean literals") {
    val t = parseBoolean("true")
    assert ( t.get == TrueLiteral)
    val f = parseBoolean("false")
    assert ( f.get == FalseLiteral)
  }

  def parseInteger(x: String) =
    GCLParser.parseAll(GCLParser.integerLiteral, x)

  test("Integer literals") {
    val x =  parseInteger("010")
    assert(x.get == 8)

    val y = parseInteger("10")
    assert(y.get == 10)

    val z = parseInteger("0x10")
    assert(z.get == 16)

    val w = parseInteger("10K")
    assert(w.get == 10240)
  }

  def parseFloat(x: String) =
    GCLParser.parseAll(GCLParser.floatLiteral, x)

  test("Doulbe literals") {
    val x =  parseFloat(".01")
    assert(x.get == new java.lang.Double("0.01"))
  }

  def parseString(x: String) = 
    GCLParser.parseAll(GCLParser.stringLiteral, x)

  test("String literals") {
    val x = parseString(""" "ab"""")
    assert(x.get == "ab")
    val y = parseString(" 'ab'")
    assert(y.get == "ab")
  }

  test("Field properties") {
    assert(GCLParser.parseAll(GCLParser.fieldProperty, "final").successful)
  }

}
