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

  test("Comments") {
    val validIds = List("a # ignore me",
      """ # ignore me
     a""", "a // ignore me",
    """// ignore me 
a""")
    assert(validIds.forall(parseIdent))
  }

  test("Invalid identifiers") {
    val invalidIds = List("0", "0a", "0_", "-a", "a b")
    assert(!invalidIds.exists(parseIdent))
  }

  def parseLiteral(x:String) = 
    GCLParser.parseAll(GCLParser.literal, x)

  test("Boolean literals") {
    val t = parseLiteral("true")
    assert (Operand.isTrue(t.get))
    val f = parseLiteral("false")
    assert (Operand.isFalse(f.get))
  }

  test("Integer literals") {
    val x =  parseLiteral("010")
    assert(Operand.isInt(x.get, 8))

    val y = parseLiteral("10")
    assert(Operand.isInt(y.get, 10))

    val z = parseLiteral("0x10")
    assert(Operand.isInt(z.get, 16))

    val w = parseLiteral("10K")
    assert(Operand.isInt(w.get, 10240))
  }

  test("String literals") {
    val x = parseLiteral(""" "ab"""")
    assert(Operand.isString(x.get,"ab"))
    val y = parseLiteral(" 'ab'")
    assert(Operand.isString(y.get,"ab"))
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

  def parseImport(x: String) =
    GCLParser.parseAll(GCLParser.importDef, x)

  test("Import") {
    val a = parseImport("import '/a/b' as b")
    assert(a.successful)
    assert(a.get.fileName == "/a/b")
    assert(a.get.as == "b")

    val b = parseImport("""import "a/b" as b""")
    assert(b.successful)
    assert(b.get.fileName == "a/b")
    assert(b.get.as == "b")
  }

  def parseIdSeq(x: String) =
    GCLParser.parseAll(GCLParser.identifierSeq, x)

  test("identifier sequence") {
    val a = parseIdSeq("a")
    assert(a.successful)
    assert(a.get == List("a"))

    val b = parseIdSeq("a.b")
    assert(b.successful)
    assert(b.get == List("a", "b"))
  }

  def parseExpression(x:String) =
    GCLParser.parseAll(GCLParser.expression, x)

  test("Factors") {
    val a = parseExpression("8")
    assert(a.successful)
    val b = parseExpression("-19")
    assert(b.successful)
  }

  test("Terms") {
    val a = parseExpression("8")
    assert(a.successful)
    val b = parseExpression("6 * 10 * 1")
    assert(b.successful)
  }

  test("Sums") {
    val a = parseExpression("8")
    assert(a.successful)
    val b = parseExpression("6 + 10 * 1")
    assert(b.successful)
    val c = parseExpression("6 + 10 + 1")
    assert(c.successful)
    val d = parseExpression("6 * 10 + 1")
    assert(d.successful)
  }

  test("Comparisons") {
    val a = parseExpression("1 >= 0")
    assert(a.successful)
    val b = parseExpression("6 + 10 * 1 == 1 + 2")
    assert(b.successful)
    val c = parseExpression("6 + 10 * 1")
    assert(c.successful)
  }

  test("Conjunction") {
    val a = parseExpression("1")
    assert(a.successful)
    val b = parseExpression("1 > 0 && 1 < 2")
    assert(b.successful)
  }

  test("Valid expressions") {
    val l = List("8", "8+1", "8+1>8", "8+1>8 && 8-1 < 8",
      "8*1>7+0 && 7>6/2 || 2>1", "(1+2)*3", "(1+2>1) && (2<0)",
      "(8*1)+1", "[] + [1]", "2 >> 1", "1 << 2", "5 % 2", "a",
      "@b", "super", "super.a", "null", " A { a = 1 }", "-A { a = 1 }")
    l.forall(parseExpression(_).successful)
  }

  def parseReference(x: String) =
    GCLParser.parseAll(GCLParser.reference, x)

  test("Valid references") {
    val l = List("@a.b", "a.b", "super", "super.a.b",
      "up.a.b", "up.up.a")
    l.forall(parseReference(_).successful)
  }

  def parseList(x: String) =
    GCLParser.parseAll(GCLParser.list, x)

  test("Lists can have optional commas") {
    val x = parseList("[1,2,]")
    assert(x.successful)
    assert(x.get.value.size == 2)
    val y = parseList("[1,2]")
    assert(y.successful)
    assert(y.get.value.size == 2)
  }

  test("Valid lists") {
    val l = List("[]", "[1+2]", "[1+2,]", "['a', 1]")
    l.forall(parseList(_).successful)
  }

  def parseStructure(x: String) =
    GCLParser.parseAll(GCLParser.structure, x)

  test("Valid structure") {
    val  l = List(
      "{ }",
      "{a = 1, b = 2}",
      "{a = 1 + 2}")
    assert(l.forall(parseStructure(_).successful))
  }

  def parseField(x: String) =
    GCLParser.parseAll(GCLParser.field, x)

  test("Valid fields") {
    val l = List("local x = 1",
      "final local b = 2",
      ".x = 1",
      "x = 1",
      "x = 1.0",
      "x = true",
      "x = 'a'",
      """x = "b"""",
      "x = [1,2]",
      "x = 1 + 2",
      "x = 'a' + 'b'",
      "local int x = 1",
      "x = {a = 1, b = '2'}")
    l.forall(parseField(_).successful)
  }

  def parseFile(x: String) =
    GCLParser.parseAll(GCLParser.file, x)

  test("Valid file") {
   val content = """ 
import 'a/b' as ab

a = 1,
b = a,
x = {
  import "c/d" as cd,
  a = 1, 
  b = "2",
  c = 1 + 2,
  e = cd,
  f = A {
    b = null,
  }
}
"""
    val f = parseFile(content)
    assert(f.successful)
    assert(f.get.size == 4)
  }

}
