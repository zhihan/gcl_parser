package me.zhihan.gcl 

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.JavaTokenParsers

object GCLParser extends JavaTokenParsers {

  // NOTE Cannot use JavaTokenParsers because "a-b" is a valid GCL
  // identifier but not a valid Java identifier
  def identifier: Parser[Types.Identifier] = """[a-zA-Z_](\w|-)*""".r ^^ { _.toString }

  def booleanLiteral: Parser[Boolean] = ("true".r ^^^ true ) | (
    "false".r ^^^ false )

  /** Integers */
  def integerLiteral: Parser[Integer] =  (hexadecimalInteger | octalInteger |
    decimalInteger) ~ integerUnit.? ^^ {
    case n ~ Some(unit) => n * unit
    case n ~ None => n
  }

  def octalInteger: Parser[Integer] = """0[0-7]+""".r ^^ { s =>
    Integer.parseInt(s.replaceAll("_",""), 8)}

  def decimalInteger: Parser[Integer] = """0|[1-9][0-9_]*""".r ^^ { s =>
    Integer.parseInt(s.replaceAll("_",""), 10)}

  def hexadecimalInteger: Parser[Integer] = """0x[0-9a-fA-F_]+""".r ^^ { s =>
    Integer.parseInt(s.replaceAll("_", "").replaceAll("0x", ""), 16)}

  // NOTE fractional integer rule is not implemented (are they even integers?)

  def integerUnit: Parser[Integer] = """[KMG]""".r ^^ {
    case "K" => 1024
    case "M" => 1024* 1024
    case "G" => 1024 * 1024 * 1024 // Likely to overflow
    // These will overflow.
    // case "T" => 1000 * 1000 * 1000 * 1000 
    // case "P" => 1000 * 1000 * 1000 * 1000 * 1000
  }

  /** Floating point numbers */
  def floatLiteral: Parser[Double] = floatingPointNumber ^^ { s =>
    new java.lang.Double(s)
  }

  /** 
    Strings 
    
    * TODO(zhihan): Handling special strings 
    */
  override def stringLiteral: Parser[String] = doubleQuotedString | singleQuotedString
  def doubleQuotedString: Parser[String] = """"[^"]*"""".r ^^ { _.toString.replaceAll("\"", "") }
  def singleQuotedString: Parser[String] = """'[^']*'""".r ^^ { _.toString.replaceAll("'", "") }

  /** TODO(zhihan): expressions */

  /** Fields */
  def fieldProperty: Parser[String] = "final" | "local" | "template" | "validation_ignore"
  def fieldProperties: Parser[List[String]] = fieldProperty.* 
  def fieldPropertiesNonEmpty: Parser[List[String]] = rep1(fieldProperty)

  // TODO(zhihan): I do not quite understand the semantics of the clause
  // fieldProperties id id 
  def fieldHeader: Parser[FieldHeader] = (fieldProperties ~ "." ~ identifier ^^ {
    case props ~ "." ~ id => FieldHeader(props, id)
  }) | (fieldProperties ~ identifier ^^ {
    case props ~ id => FieldHeader(props, id)
  })

  /** Expansions */
  def signatureList: Parser[List[Types.Identifier]] = ("""\(\w*\)""".r ^^ { 
    _ => List[Types.Identifier]()
  }) | ("(" ~ identifier ~ ("," ~ identifier).* ~ ")" ^^ {
    case _ ~ h ~ pairList ~ _ => {
      h :: (pairList map { case _ ~ id => id })
    }
  })

  def signature: Parser[List[Types.Identifier]] = signatureList.? ^^ {
    case Some(listId) => listId
    case None => List[Types.Identifier]()
  }

  /** Import statements */
  def importDef: Parser[Import] = "import" ~ stringLiteral ~ "as" ~ identifier ^^ {
    case _ ~ fileName ~ _ ~ id => Import(fileName, id)
  }

  /** Full path identifier */
  def identifierSeq: Parser[List[Types.Identifier]] = identifier ~ ("." ~ identifier).* ^^ {
    case h ~ pairList =>
      h :: (pairList map {case _ ~ id  => id})
  }

  /** Operators */
  def relationalOperator: Parser[Types.RelOp] = """==|>=|<=|<|>|!=""".r ^^ { _.toString }
  def additionalOperator: Parser[Types.AdditionalOp] = """+|-""".r ^^ { _.toString }
  def multiplicativeOperator: Parser[Types.MultiplicativeOp] = """*|/""".r ^^ {_.toString}
  def unaryOperator: Parser[Types.UnaryOp] = "!|-".r ^^ { _.toString }

  def operand: Parser[Operand] = integerLiteral ^^ { IntegerLiteral(_) }
  def factor: Parser[Factor] = (unaryOperator ~ operand ^^ {
    case op ~ v => Factor(v, Some(op), None)
  }) | (operand ^^ { v => Factor(v, None, None)})
}
