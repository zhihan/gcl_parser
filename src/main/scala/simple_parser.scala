package me.zhihan.parsers


import scala.util.parsing.combinator._

object SimpleParser extends RegexParsers {

  def word: Parser[String] = """[a-z]+""".r ^^ { _.toString }

}
