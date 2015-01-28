package me.zhihan.gcl

object Types {
  type Identifier = String
  type FieldProperties = List[String]
  type FileName = String
}

/* Field definitions */
abstract class FieldProperty 
case object Final extends FieldProperty {}
case object Local extends FieldProperty {}
case object Template extends FieldProperty {}
case object ValidationIgore extends FieldProperty {}


case class Field(
  val fieldHeader: FieldHeader,
  val value: FieldValue,
  val expansionInvocation: Option[ExpansionInvocation]
) {}

case class FieldValue() {
}

case class ExpansionInvocation() {
}

case class StringLiteral(val value:String) {}

case class FieldHeader(
  val props: Types.FieldProperties,
  val id: Types.Identifier) {}

/** Import statement */
case class Import(
  val fileName: Types.FileName,
  val as: Types.Identifier) {}


