package SatExample

import org.sat4j.core._
import org.sat4j.minisat._
import scala.collection.mutable.HashMap

trait ISolver
{
  def addClause(cl : Clause) : Unit = {
    addClause_(cl.lits)
  }

  def addClause(lits: Literal*) : Unit = {
    addClause_(lits)
  }

  def solve() : Boolean = {
    solve(Seq.empty)
  }

  def modelValue(l : Literal) : Boolean = {
    val b = modelValue(l.variable)
    l match {
      case PositiveLiteral(_) => b
      case NegativeLiteral(_) => !b
    }
  }

  // to be implemented by sub-class
  def addClause_(lits: Seq[Literal]) : Unit
  def solve(assumps: Seq[Literal]) : Boolean
  def modelValue(v : Int) : Boolean
}

class Solver extends ISolver
{
  val solver = SolverFactory.newDefault()

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
}
