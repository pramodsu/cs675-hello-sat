package SatExample

import org.sat4j.core._
import org.sat4j.minisat._

object Main {
  def main(args: Array[String]): Unit = {
    val a = Literal.create(1)
    val b = Literal.create(2)
    val an = ~a
    val bn = ~b

    val S = new Solver()
    S.addClause(a, b)
    S.addClause(~a, ~b)
    // Solve
    println(S.solve())
    // Extract model.
    val va = S.modelValue(a)
    val vb = S.modelValue(b)
    println("model: [a -> %s, b -> %s]".format(va.toString(), vb.toString()))
  }
}
