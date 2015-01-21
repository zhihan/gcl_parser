package me.zhihan.gcl 

import scala.util.parsing.combinator._

object Parser extends RegexParsers {
  // NOTE Cannot use JavaTokenParsers because "a-b" is a valid GCL
  // identifier but not a valid Java identifier
  def identifier: Parser[Types.Identifier] = """[a-zA-Z_](\w|-)*""".r ^^ { _.toString }

  def booleanLiteral: Parser[BooleanLiteral] = ("true".r ^^^ TrueLiteral ) | (
    "false".r ^^^ FalseLiteral )

  def integerLiteral: Parser[Integer] =  hexadecimalInteger | octalInteger | decimalInteger 

  def octalInteger: Parser[Integer] = """0[0-7]+""".r ^^ { s =>
    Integer.parseInt(s.replaceAll("_",""), 8)}
  def decimalInteger: Parser[Integer] = """0|[1-9][0-9_]*""".r ^^ { s =>
    Integer.parseInt(s.replaceAll("_",""), 10)}
  def hexadecimalInteger: Parser[Integer] = """0x[0-9a-fA-F_]+""".r ^^ { s =>
    Integer.parseInt(s.replaceAll("_", "").replaceAll("0x", ""), 16)}
}
