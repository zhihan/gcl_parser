package me.zhihan.gcl 

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.JavaTokenParsers

/**
  * GCLParser - A parser for Google Generic Configuration Language
  * (GCL)
  * 
  * A parser for GCL written with Scala parser combinators. A few
  * simplifications are made based on my understanding of the GCL
  * language specification. See the details in the comments of the
  * parsing rules.
  * 
  */

object GCLParser extends JavaTokenParsers {

  /** 
    * Identifiers
    * 
    * Identifiers must begin with a letter and followed by alphanumeric
    *  or dash ('-')
    * e.g.,
    *  a, _a, _1, a-b
    */ 
  // NOTE Cannot use JavaTokenParsers because "a-b" is a valid GCL
  // identifier but not a valid Java identifier
  def identifier: Parser[Types.Identifier] =
    """[a-zA-Z_](\w|-)*""".r ^^ { _.toString }

  /** Full path identifier */
  def identifierSeq: Parser[List[Types.Identifier]] = identifier ~ ("." ~ identifier).* ^^ {
    case h ~ pairList =>
      h :: (pairList map {case _ ~ id  => id})
  }

  /**
    * Literals
    * 
    * The following literals are supported
    * 
    *  - Integers: we support 32-bit integers in octal, decimal or
    *    hexadecimal format. Integer unit are supported in limited
    *    format to avoid overflowing. 
    * 
    *  - Strings: strings can be either single quoted or double
    *    quoted. Special strings, e.g., '\n' is not yet supported. 
    * 
    *  - Booleans: true or false.
    * 
    * Incompatibilities
    * 
    * o GCL supports 'fractional integers', I do not understand what
    * it is.
    * 
    * o GCL syntax supports floating point numbers, which is not
    *   yet supported by this parser.
    */
  def literal: Parser[Operand] = (stringLiteral ^^ {
    StringLiteral(_) }) | (booleanLiteral ^^ {
      BooleanLiteral(_)}) | (integerLiteral ^^ {
        IntegerLiteral(_)})

  private def booleanLiteral: Parser[Boolean] = ("true".r ^^^ true ) | (
    "false".r ^^^ false )

  /** Integers */
  private def integerLiteral: Parser[Integer] =
    (hexadecimalInteger | octalInteger |
      decimalInteger) ~ integerUnit.? ^^ {
      case n ~ Some(unit) => n * unit
      case n ~ None => n
    }

  private def octalInteger: Parser[Integer] = """0[0-7]+""".r ^^ { s =>
    Integer.parseInt(s.replaceAll("_",""), 8)}

  private def decimalInteger: Parser[Integer] = """0|[1-9][0-9_]*""".r ^^ {
    s =>
    Integer.parseInt(s.replaceAll("_",""), 10)}

  private def hexadecimalInteger: Parser[Integer] =
    """0x[0-9a-fA-F_]+""".r ^^ { s =>
      Integer.parseInt(s.replaceAll("_", "").replaceAll("0x", ""), 16)}

  // NOTE fractional integer rule is not implemented (are they even integers?)
  private def integerUnit: Parser[Integer] = """[KMG]""".r ^^ {
    case "K" => 1024
    case "M" => 1024* 1024
    case "G" => 1024 * 1024 * 1024 // Likely to overflow
    // These will overflow.
    // case "T" => 1000 * 1000 * 1000 * 1000 
    // case "P" => 1000 * 1000 * 1000 * 1000 * 1000
  }

  /** Floating point numbers */
  // TODO(zhihan): This is not currently supported as Scala's built-in
  // float literal in JavaTokenParser will also accept integers.
  private def floatLiteral: Parser[Double] = floatingPointNumber ^^ { s =>
    new java.lang.Double(s)
  }

  /**
    * Strings 
    */
  //TODO(zhihan): Handling special strings
  override def stringLiteral: Parser[String] =
    doubleQuotedString | singleQuotedString
  def doubleQuotedString: Parser[String] = """"[^"]*"""".r ^^ {
    _.toString.replaceAll("\"", "") }
  def singleQuotedString: Parser[String] = """'[^']*'""".r ^^ {
    _.toString.replaceAll("'", "") }

  /**
    * Expressions
    * 
    * Expressions consists of composition of simple values (operands) with the
    * following operations.
    *  
    *  - Unary operations ! (not), - (negation)
    *  - Logical conjunctions (&&) and disjunctions ||
    *  - Comparisons (==, <=, >=, >, <, !=)
    *  - Arithmetic operations ( +, -, *, /)
    * 
    * Incompatibilities:
    * 
    *  o No lambda expressions.
    *  
    *  o Due to the algorithm of the parser, a sequence of more than two
    *  operators are parsed associated to the right, e.g., a * b * c
    *  is parsed as a * (b * c). This should not affect interpreted
    *  results.
    * 
    */

  /** Expression */
  def expression: Parser[Types.Expression] = conjunction ~ ("||" ~ conjunction).* ^^ {
    case d ~ pairList => Disjunction(d :: (pairList map {case _ ~ x => x}))
  }

  /** Operators */
  private def relationalOperator: Parser[Types.RelOp] =
    """==|>=|<=|<|>|!=""".r ^^ { _.toString }

  private def additiveOperator: Parser[Types.AdditiveOp] =
    """\+|-""".r ^^ { _.toString }

  private def multiplicativeOperator: Parser[Types.MultiplicativeOp] =
    """\*|/|<<|>>|%""".r ^^ {_.toString}

  private def unaryOperator: Parser[Types.UnaryOp] = "!|-".r ^^ { _.toString }

  private def factor: Parser[Factor] = (unaryOperator ~ operand ^^ {
    case op ~ v => Factor(v, Some(op), None)
  }) | (operand ^^ { v => Factor(v, None, None)})

  // NOTE
  // The parser forms the expression tree by recursing right, instead of left.
  // May require a tree rewriting.
  private def term: Parser[Term] = (factor ~ multiplicativeOperator ~ term ^^ {
    case f ~ op ~ t => BinaryTerm(op, f, t)
  }) | (factor ^^ {
    SimpleTerm(_)
  })

  private def _sum: Parser[Sum] = (term ~ additiveOperator ~ _sum ^^ {
    case t ~ op ~ s => BinarySum(op, t, s)
  }) | (term ^^ {
    SimpleSum(_)
  })

  private def comparison: Parser[Comparison] =
    (_sum ~ relationalOperator ~ _sum ^^ {
      case l ~ op ~ r => Comp(op, l, r)
    }) | (_sum ^^ { SimpleComp(_) })

  private def conjunction: Parser[Conjunction] =
    comparison ~ ("&&" ~ comparison).* ^^ {
      case c ~ pairList => Conjunction(c :: (pairList map { case _ ~ c => c }))
  }

  /** 
    * Lists
    * 
    *  Lists are list of empty or nonempty entries enclosed in squre
    * brackets.  For example, [1,2], ['a', 'b']. Lists can be operands
    * and used in expressions, e.g., [1,2] + [3]. Lists can contain
    * expressions as its entries, e.g., [1 + 2]. Lists can have an
    * optional comma at the end, i.e., [1,2,] and [1,2] are parsed
    * equivalently.
    * 
    */

  /** Lists */
  def list: Parser[ListExpression] = ("[" ~ nonemptyList ~ "]" ^^ {
    case _ ~ l ~ _ => l}) | ("""\[\s*\]""".r ^^ { _ => ListExpression(List())}) 

  private def nonemptyList: Parser[ListExpression] = expression ~ (
    "," ~ expression).* ~ ",?".r ^^ {
    case e ~ pairList ~ _ => ListExpression(e :: (pairList map { case _ ~ e => e }))
  }

  /** Fields */
  private def fieldProperty: Parser[String] =
    "final" | "local" | "template" | "validation_ignore"

  def fieldProperties: Parser[List[String]] = fieldProperty.*

  def fieldPropertiesNonEmpty: Parser[List[String]] = rep1(fieldProperty)
 
  // TODO(zhihan): I do not quite understand the semantics of the clause
  // fieldProperties id id

  def fieldHeader: Parser[FieldHeader] = (fieldProperties ~ "." ~ identifier ^^ {
    case props ~ "." ~ id => FieldHeader(props, id)
  }) | (fieldProperties ~ identifier ^^ {
    case props ~ id => FieldHeader(props, id)
  })

  def value: Parser[Value] = ( "=" ~ expression ^^ {
    case _ ~ e => Value(e)
  })

  def field: Parser[Field] = fieldHeader ~ value ^^ {
    case h ~ v => Field(h, v)
  }

  /** 
    * Structure 
    */
  def structure: Parser[Structure] = ("{" ~ nonemptyEntryList ~ "}" ^^ {
    case _ ~ l ~ _ => { Structure(l) } 
  }) | ("""\{\s*\}""".r ^^ { _ => Structure(List[Field]()) })

  /** 
    *  Operand 
    */
  private def operand: Parser[Operand] = ("(" ~ expression ~ ")" ^^ {
    case _ ~ e ~ _ => e
  }) | literal | list | structure

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

  private def entry: Parser[Entry] = field | importDef

  private def nonemptyEntryList: Parser[List[Entry]] =
    rep1(entry ~ ",?".r) ^^ {
      _.map {case f ~ _ => f}
  }

  /** 
    * File
    * 
    * A file is simply a nonempty list of entries. 
    */
  def file: Parser[List[Entry]] = nonemptyEntryList

}
