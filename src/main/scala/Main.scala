import org.sat4j.core._
import org.sat4j.minisat._


object SatObj {
  val sat4j : org.sat4j.specs.ISolver = SolverFactory.newDefault()
  val clauses = new Vec[VecInt]()

  def addOutput(g: Gate) : Unit = {
    val cls = new VecInt(Array(g.satvarId))
    SatObj.clauses.push(cls)
    SatObj.sat4j.addClause(cls)
  }

  def gate2cnf(g: Gate) : Unit = {
    g match {
      case and : AndGate =>
        var cls = new VecInt(Array (-and.in1Id, -and.in2Id, and.satvarId))
        SatObj.clauses.push(cls)
        SatObj.sat4j.addClause(cls)

        cls = new VecInt(Array (and.in1Id, -and.satvarId))
        SatObj.clauses.push(cls)
        SatObj.sat4j.addClause(cls)

        cls = new VecInt(Array (and.in2Id, -and.satvarId))
        SatObj.clauses.push(cls)
        SatObj.sat4j.addClause(cls)

        gate2cnf(and.in1)
        gate2cnf(and.in2)

      case or : OrGate =>
        var cls = new VecInt(Array (or.in1Id, or.in2Id, -or.satvarId))
        SatObj.clauses.push(cls)
        SatObj.sat4j.addClause(cls)

        cls = new VecInt(Array (-or.in1Id, or.satvarId))
        SatObj.clauses.push(cls)
        SatObj.sat4j.addClause(cls)

        cls = new VecInt(Array (-or.in2Id, or.satvarId))
        SatObj.clauses.push(cls)
        SatObj.sat4j.addClause(cls)

        gate2cnf(or.in1)
        gate2cnf(or.in2)

      case not : NotGate =>
        var cls = new VecInt(Array (-not.inId, -not.satvarId))
        SatObj.clauses.push(cls)
        SatObj.sat4j.addClause(cls)

        cls = new VecInt(Array (not.inId, not.satvarId))
        SatObj.clauses.push(cls)
        SatObj.sat4j.addClause(cls)

        gate2cnf(not.in1)

      case _  => // do nothing
    }
  }
}

abstract class Gate {
  var satvarId : Int = SatObj.sat4j.nextFreeVarId(true)
}

case class Inport() extends  Gate {
  override def toString: String = satvarId.toString
}

case class NotGate(in1: Gate) extends Gate {
  var inId : Int = in1.satvarId
  override def toString: String =  "~" + in1.toString
}

case class AndGate(in1: Gate, in2: Gate) extends Gate {
  var in1Id : Int = in1.satvarId
  var in2Id : Int = in2.satvarId
  override def toString: String = "(" + in1.toString + " & " + in2.toString + ")"
}

case class OrGate(in1: Gate, in2: Gate) extends Gate {
  var in1Id : Int = in1.satvarId
  var in2Id : Int = in2.satvarId
  override def toString: String = "(" + in1.toString + " | " + in2.toString + ")"
}


object Main {

  def main(args: Array[String]): Unit = {
    val in1 =  Inport()
    val in2 =  Inport()
    val in3 =  Inport()

    val a =  AndGate(NotGate(in1), in2)
    val b =  AndGate(NotGate(in2), in1)
    val c =  AndGate(NotGate(in2), in3)

    val d = OrGate(a,b)
    val e = OrGate(d,c)

    SatObj.addOutput(e) // to insert (e <-> true) clause

    print("Formula : ")
    println(e)

    SatObj.gate2cnf(e) // converts boolean expression tree to cnf clauses

    if (SatObj.sat4j.isSatisfiable) {
      println("SATISFAIBLE")
      val nvars = SatObj.sat4j.nVars()
      val cl = SatObj.sat4j.model.iterator

      println("Total variables : " + nvars)
      print("SAT Assignments : ")
      while(cl.hasNext) {
        print(cl.next() + " ")
      }
      println()
      println("-----------------------")
      println("Clauses")
      println("-----------------------")

      val vit = SatObj.clauses.iterator()
      while (vit.hasNext) {
        println(vit.next())
      }
      println("-----------------------")

    }
    else {
      println("UN-SATISFAIBLE")
    }

  }
}
