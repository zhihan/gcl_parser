package me.zhihan.gcl 

import scala.util.parsing.combinator._

object Parser extends RegexParsers {
  // NOTE Cannot use JavaTokenParsers because "a-b" is a valid GCL
  // identifier but not a valid Java identifier
  def identifier: Parser[Types.Identifier] = """[a-zA-Z_](\w|-)*""".r ^^ { _.toString }


  def booleanLiteral: Parser[BooleanLiteral] = ("true".r ^^^ TrueLiteral ) | (
    "false".r ^^^ FalseLiteral )


}
