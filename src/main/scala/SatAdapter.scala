package SatExample

import scala.collection.mutable.HashMap

abstract class Literal {
  val variable : Int
  def unary_~() : Literal
  def toInt : Int
}

case class PositiveLiteral(v: Int) extends Literal {
  override def toString() : String = v.toString()
  override val variable = v
  override def toInt = v
  def unary_~() : Literal = NegativeLiteral(v)
}

case class NegativeLiteral(v :Int) extends Literal {
  override def toString() : String = "-" + v.toString()
  override val variable = v
  override def toInt = -v
  def unary_~() : Literal = PositiveLiteral(v)
}

object Literal {
  def create(v : Int) = PositiveLiteral(v)
}

case class Clause(lits: List[Literal]) {
  override  def toString() : String = 
    lits.map(_.toString()).mkString("(", " + ", ")")
}
