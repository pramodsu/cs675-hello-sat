package SatExample

abstract class Literal {
  def variable() : String
}

case class PositiveLiteral(name: String) extends Literal {
  override def toString() : String = name
  override def variable() = name
}

case class NegativeLiteral(name: String) extends Literal {
  override def toString() : String = "~" + name
  override def variale() = name
}

case class Clause(lits: List[Literal]) {
}
