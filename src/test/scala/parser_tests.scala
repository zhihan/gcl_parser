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

  def isRelOp(x:String) =
    GCLParser.parseAll(GCLParser.relationalOperator, x).successful
  test("Relational operator") {
    val ops = List("<", ">", "<=", ">=", "!=", "==")
    assert(ops.forall(isRelOp))
  }

  def parseFactor(x:String) =
    GCLParser.parseAll(GCLParser.factor, x)
  test("Factors") {
    val a = parseFactor("8")
    assert(a.successful)
    val b = parseFactor("-19")
    assert(b.successful)
    assert(b.get.op == Some("-"))
  }

  def parseTerm(x:String) =
    GCLParser.parseAll(GCLParser.term, x)
  test("Terms") {
    val a = parseTerm("8")
    assert(a.successful)
    val b = parseTerm("6 * 10 * 1")
    assert(b.successful)
  }

  def parseSum(x:String) =
    GCLParser.parseAll(GCLParser._sum, x)
  test("Sums") {
    val a = parseSum("8")
    assert(a.successful)
    val b = parseSum("6 + 10 * 1")
    assert(b.successful)
    val c = parseSum("6 + 10 + 1")
    assert(c.successful)
    val d = parseSum("6 * 10 + 1")
    assert(d.successful)
  }

  def parseComparison(x: String) =
    GCLParser.parseAll(GCLParser.comparison, x)
  test("Comparisons") {
    val a = parseComparison("1 >= 0")
    assert(a.successful)
    val b = parseComparison("6 + 10 * 1 == 1 + 2")
    assert(b.successful)
    val c = parseComparison("6 + 10 * 1")
    assert(c.successful)
  }

  def parseConjunction(x: String) =
    GCLParser.parseAll(GCLParser.conjunction, x)
  test("Conjunction") {
    val a = parseConjunction("1")
    assert(a.successful)
    val b = parseConjunction("1 > 0 && 1 < 2")
    assert(b.successful)
  }

  def parseExpression(x: String) =
    GCLParser.parseAll(GCLParser.expression, x)

  test("Valid expressions") {
    val l = List("8", "8+1", "8+1>8", "8+1>8 && 8-1 < 8",
      "8*1>7+0 && 7>6/2 || 2>1", "(1+2)*3", "(1+2>1) && (2<0)",
      "(8*1)+1")
    l.forall(parseExpression(_).successful)
  }

  def parseList(x: String) =
    GCLParser.parseAll(GCLParser.list, x)

  test("Valid lists") {
    val x = parseList("[1,2,]")
    assert(x.successful)
    assert(x.get.value.size == 2)
    val y = parseList("[1,2]")
    assert(y.successful)
    assert(y.get.value.size == 2)
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
      "x = 1",
      "x = 1.0",
      "x = true",
      "x = 'a'",
      """x = "b"""",
      "x = [1,2]",
      "x = 1 + 2",
      "x = 'a' + 'b'",
      "x = {a = 1, b = '2'}")
    l.forall(parseField(_).successful)
  }

  def parseFile(x: String) =
    GCLParser.parseAll(GCLParser.file, x)

  test("Valid file") {
   val content = """ 
a = 1,
b = 2,
x = {
  a = 1, 
  b = "2",
  c = 1 + 2
}
"""
    val f = parseFile(content)
    assert(f.successful)
    assert(f.get.size == 3)
  }

}
