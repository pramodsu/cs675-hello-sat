package SatExample

import org.sat4j.core._
import org.sat4j.minisat._
import scala.collection.mutable.HashMap

class Solver()
{
  val solver = SolverFactory.newDefault()

  def addClause(cl : Clause) : Unit = {
    addClause_(cl.lits)
  }

  def addClause(lits: Literal*) : Unit = {
    addClause_(lits)
  }

  def addClause_(lits : Seq[Literal]) : Unit = {
    // create new variables, if not already present.
    lits.foreach {
      l => {
        if (l.variable > solver.nVars()) {
          solver.newVar(l.variable - solver.nVars())
        }
      }
    }
    // map into an array.
    val vec = new VecInt(lits.map(_.toInt).toArray)
    // add to solver.
    solver.addClause(vec)
  }


  def solve() : Boolean = {
    solver.isSatisfiable()
  }

  def solve(assumps: Seq[Literal]) : Boolean = {
    if (assumps.size == 0) {
      solver.isSatisfiable()
    } else {
      val array = assumps.map(_.toInt).toArray 
      solver.isSatisfiable(new VecInt(array))
    }
  }

  def modelValue(v : Int) : Boolean = {
    solver.model(v)
  }

  def modelValue(l : Literal) = {
    val b = solver.model(l.variable)
    l match {
      case PositiveLiteral(_) => b
      case NegativeLiteral(_) => !b
    }
  }
}
