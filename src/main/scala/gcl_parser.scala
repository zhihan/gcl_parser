package me.zhihan.gcl 

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.RegexParsers

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

object GCLParser extends RegexParsers {
  /** 
    * Comments 
    * 
    * Comments are C++ like, i.e., //... until the end of line, or
    *  python like, i.e., #... until the end of line.
    */
  protected override val whiteSpace = """(\#.*|\/\/.*|\s)+""".r

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
  def identifierSeq: Parser[List[Types.Identifier]] = identifier ~
    ("." ~ identifier).* ^^ {
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
  /* private def floatLiteral: Parser[Double] = floatingPointNumber ^^ { s =>
    new java.lang.Double(s)
  } */

  /**
    * Strings 
    */
  //TODO(zhihan): Handling special strings
  def stringLiteral: Parser[String] =
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

  private def factor: Parser[Factor] = (unaryOperator ~ operand ~ structure ^^ {
    case op ~ v ~ s => Factor(v, Some(op), Some(s))
  }) | (unaryOperator ~ operand ^^ {
    case op ~ v => Factor(v, Some(op), None)
  })  | (operand ~ structure ^^ {
    case v ~ s => Factor(v, None, Some(s)) 
  })  | (operand ^^ {
    v => Factor(v, None, None)
  })

  private def term: Parser[Term] = factor ~
    (multiplicativeOperator ~ factor).* ^^ {
      case f ~ pairList => Term(f, pairList.map {
        case op ~ f => (op, f) })
    }
      

  private def _sum: Parser[Sum] = term ~
    (additiveOperator ~ term).* ^^ {
      case t ~ pairList => Sum(t, pairList.map {
        case op ~ t => (op, t) })
    }
 
  private def comparison: Parser[Comparison] =
    (_sum ~ relationalOperator ~ _sum ^^ {
      case l ~ op ~ r => Comp(op, l, r)
    }) | (_sum ^^ { SimpleComp(_) })

  private def conjunction: Parser[Conjunction] =
    comparison ~ ("&&" ~ comparison).* ^^ {
      case c ~ pairList => Conjunction(c :: (pairList map { case _ ~ c => c }))
  }

  /**
    * References 
    * 
    * Three types of references are supported: absolute reference ('@a.b'),
    *  relative reference ('up.a.b') or super reference ('super.a.b').
    */
  def reference: Parser[Reference] = superReference | relativeReference | absoluteReference

  private def upReference: Parser[UpReference] =
    ("""up\.""".r ~ upReference ^^ {
      case _ ~ r => UpReference(r)
    }) | ("""up\.""".r ~ relativeReference ^^ {
      case _ ~ r => UpReference(r)
    })

  private def relativeReference: Parser[RelativeReference] =
    identifierSeq ^^ { RelativeReference(_) }

  private def superReference: Parser[SuperReference] =
    ("""super\.""".r ~ identifierSeq ^^ {
      case _ ~ l => SuperReference(l)
    }) | ("""super""" ^^ {
      _ => SuperReference(List[Types.Identifier]())
    })

  private def absoluteReference: Parser[AbsoluteReference] =
    "@" ~ identifierSeq ^^ {
      case _ ~ l => AbsoluteReference(l)
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
    */

  /** Lists */
  def list: Parser[ListExpression] = ("[" ~ nonemptyList ~ "]" ^^ {
    case _ ~ l ~ _ => l}) | ("""\[\s*\]""".r ^^ { _ => ListExpression(List())}) 

  private def nonemptyList: Parser[ListExpression] = expression ~ (
    "," ~ expression).* ~ ",?".r ^^ {
    case e ~ pairList ~ _ => ListExpression(e :: (pairList map { case _ ~ e => e }))
  }

  /**
    *  Operand 
    */
  private def operand: Parser[Operand] = ("(" ~ expression ~ ")" ^^ {
    case _ ~ e ~ _ => e
  }) | ("null" ^^ {_ => Null }) | literal | list | structure | reference

  /** 
    * Fields 
    * 
    *  Fields are part of a structure. For example
    * 
    *    x = 'some_file',
    *    local y = 10
    *    local service x = T  ('service' is the type)
    * 
    *  Root-level fields, i.e., fields that are not enclosed in any
    *  structures are associated with the 'root' structure of the
    *  file. Fields can have properties and headers. 
    */
  def field: Parser[Field] = fieldHeader ~ value ^^ {
    case h ~ v => Field(h, v)
  }

  private def fieldProperty: Parser[String] =
    "final" | "local" | "template" | "validation_ignore"

  private def fieldProperties: Parser[List[String]] = fieldProperty.*

  // TODO(zhihan): I do not quite understand the semantics of the clause
  // fieldProperties id id.
  private def fieldHeader: Parser[FieldHeader] = (fieldProperties ~ "." ~ identifier ^^ {
    case props ~ "." ~ id => FieldHeader(props, None, id)
  }) | (fieldProperties ~ identifier ~ identifier ^^ {
    case props ~ t ~ id => FieldHeader(props, Some(t), id)
  }) | (fieldProperties ~ identifier ^^ {
    case props ~ id => FieldHeader(props, None, id)
  })

  private def value: Parser[Value] = "=" ~ expression ^^ {
    case _ ~ e => Value(e)
  }

  /** 
    * Structure 
    * 
    * Structures are groups of entities (field, check, expansion or
    * import statements). Structures are hierarchical, i.e., a
    * structure can contain other structures. Every GCL file has a
    * root-level structure associated with the file.
    */
  def structure: Parser[Structure] = ("{" ~ nonemptyEntryList ~ "}" ^^ {
    case _ ~ l ~ _ => { Structure(l) } 
  }) | ("""\{\s*\}""".r ^^ { _ => Structure(List[Entry]()) })

  private def nonemptyEntryList: Parser[List[Entry]] =
    rep1(entry ~ ",?".r) ^^ {
      _.map {case f ~ _ => f}
  }

  private def entry: Parser[Entry] = field | importDef


  /** 
    *  Import statements 
    * 
    *  Import statement import a structure and assign it to a local
    *  variable, e.g.,
    * 
    *    import 'path/to/file.gcl' as x 
    */
  def importDef: Parser[Import] = "import" ~ stringLiteral ~ "as" ~ identifier ^^ {
    case _ ~ fileName ~ _ ~ id => Import(fileName, id)
  }

 
  /** 
    *  Expansions 
    * 
    */
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

  /** 
    * File
    * 
    * A file is simply a nonempty list of entries. 
    */
  def file: Parser[List[Entry]] = nonemptyEntryList

}
