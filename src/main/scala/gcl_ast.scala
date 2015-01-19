package me.zhihan.gcl

object Types {
  type Identifier = String

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


case class FieldHeader(
  val id: Types.Identifier,
  val properties: List[FieldProperty]
) {}


case class FieldValue() {
}

case class ExpansionInvocation() {
}
