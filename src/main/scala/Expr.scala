package SatExample

abstract class Expr

case class BoolLit(value: Boolean) extends Expr {
  override def toString = value.toString()
}

case class Variable(name: String) extends Expr {
  override def toString(): String = name
} 

case class And(args: List[Expr]) extends Expr {
  override def toString(): String = {
    args.map(x => x.toString()).mkString("("," && ",")")
  }
}

case class Or(args: List[Expr]) extends Expr {
  override def toString(): String = {
    args.map(x => x.toString()).mkString("("," || ",")")
  }
}

case class Not(arg: Expr) extends Expr {
  override def toString(): String = {
    "~" + "(" + arg.toString() + ")"
  }
}
