package SatExample

import org.sat4j.core._
import org.sat4j.minisat._

object Main {
  def main(args: Array[String]): Unit = {
    val a = Literal.create(1)
    val b = Literal.create(2)
    val an = ~a
    val bn = ~b

    println("a = " + a.toString())
    println("an = " + an.toString())

    val S = new Solver()
    S.addClause(a, b)
    S.addClause(~a, ~b)
    // Solve
    println(S.solve(List(a)))
    // Extract model.
    var va = S.modelValue(a)
    var vb = S.modelValue(b)
    println("model: [a -> %s, b -> %s]".format(va.toString(), vb.toString()))

    var la = if (va) ~a else a
    var lb = if (vb) ~b else b
    S.addClause(la, lb)

    println(S.solve())
    // Extract model.
    va = S.modelValue(a)
    vb = S.modelValue(b)
    println("model: [a -> %s, b -> %s]".format(va.toString(), vb.toString()))

    la = if (va) ~a else a
    lb = if (vb) ~b else b
    S.addClause(la, lb)

  }
}
