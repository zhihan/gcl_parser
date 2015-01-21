package me.zhihan.gcl 

import org.scalatest.FunSuite

class ParserTests extends FunSuite {
  // Helper function to parse identifiers
  def parseIdent(x:String) =
    Parser.parseAll(Parser.identifier, x).successful

  test("Valid identifiers") {
    val validIds = List("a", "_a", "_", "_1", "a-b")
    assert(validIds.forall(parseIdent))
  }

  test("Invalid identifiers") {
    val invalidIds = List("0", "0a", "0_", "-a", "a b")
    assert(!invalidIds.exists(parseIdent))
  }

  def parseBoolean(x:String) = 
    Parser.parseAll(Parser.booleanLiteral, x)

  test("Boolean literals") {
    val t = parseBoolean("true")
    assert ( t.get == TrueLiteral)
    val f = parseBoolean("false")
    assert ( f.get == FalseLiteral)
  }

  def parseInteger(x: String) =
    Parser.parseAll(Parser.integerLiteral, x)

  test("Integer literals") {
    val x =  parseInteger("010")
    assert(x.get == 8)

    val y = parseInteger("10")
    assert(y.get == 10)

    val z = parseInteger("0x10")
    assert(z.get == 16)
  }
}
