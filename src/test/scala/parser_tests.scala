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
    assert ( t.get == true)
    val f = parseBoolean("false")
    assert ( f.get == false)
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

    assert(GCLParser.parseAll(GCLParser.fieldProperties, "final").successful)
    assert(GCLParser.parseAll(GCLParser.fieldProperties, "").successful)
    assert(GCLParser.parseAll(GCLParser.fieldProperties, "final local").successful)

    assert(GCLParser.parseAll(
      GCLParser.fieldPropertiesNonEmpty, "final").successful)
    assert(!GCLParser.parseAll(
      GCLParser.fieldPropertiesNonEmpty, "").successful)
    assert(GCLParser.parseAll(
      GCLParser.fieldPropertiesNonEmpty, "final local").successful)
  }

  def parseFieldHeader(x:String) =
    GCLParser.parseAll(GCLParser.fieldHeader, x)

  test("Field header") {
    val a = parseFieldHeader(" .a")
    assert(a.successful)
    assert(a.get.props.size == 0)
    assert(a.get.id == "a")

    val b = parseFieldHeader("final local b")
    assert(b.successful)
    assert(b.get.props.size == 2)
    assert(b.get.id == "b")
  }

  def parseSignature(x:String) =
    GCLParser.parseAll(GCLParser.signature, x)

  test("Signature") {
    val a = parseSignature("()")
    assert(a.successful)
    assert(a.get.size == 0)
    val b = parseSignature("")
    assert(b.successful)
    assert(b.get.size == 0)

    val c = parseSignature("(a, b)")
    assert(c.successful)
    println(c.get)
    assert(c.get.size == 2)

  }

}
