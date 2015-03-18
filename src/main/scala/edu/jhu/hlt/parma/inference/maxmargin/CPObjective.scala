package edu.jhu.hlt.parma.inference.maxmargin

import collection.mutable.ArrayBuffer
import ilog.concert._
import ilog.cplex._

case class Restriction(val x1Pos: Boolean, val x2Pos: Boolean)
object DefaultRestriction extends Restriction(true, true)

/**
 * allows you to abstract away the encoding given to cplex
 */
class CPObjective(val cplex: IloCplex, val useQuadConstraints: Boolean) {

    lazy val qpObj: IloLQNumExpr = cplex.lqNumExpr()  		   // quadratic objective, no constraints
    lazy val qcObj: IloLinearNumExpr = cplex.linearNumExpr()   // quadratic constraints, linear objective
    val newVariables = new ArrayBuffer[IloIntVar]

	private var const = 0d
	def getConstant: Double = const

    def getObjective: Either[IloLinearNumExpr, IloQuadNumExpr] = {
        if(useQuadConstraints) {
			qcObj.setConstant(this.const)
			Left(qcObj)
		}
		else {
			qpObj.setConstant(this.const)
			Right(qpObj)
		}
	}

    def linearTerm(coef: Double, variable: IloIntVar) {
        if(useQuadConstraints)
            qcObj.addTerm(coef, variable)
        else
            qpObj.addTerm(coef, variable)
    }

	def addConstant(const: Double) {
		this.const += const
	}

	/**
	 * generalizes `objective += coef * max(vars)`
	 */
	def linearTermWithMax(coef: Double, vars: Seq[IloIntVar]): IloIntVar = {
		val mv = cplex.boolVar()
		for(v <- vars) cplex.addGe(mv, v)
		newVariables += mv
		linearTerm(coef, mv)
		mv
	}

    def quadTerm(coef: Double, x1: IloIntVar, x2: IloIntVar, r: Restriction = DefaultRestriction) {
        if(useQuadConstraints) {
            val v = cplex.boolVar()
            if(r.x1Pos) cplex.addGe(v, x1)
            else cplex.addLe(v, x1)
            if(r.x2Pos) cplex.addGe(v, x2)
            else cplex.addLe(v, x2)
            qcObj.addTerm(coef, v)
            newVariables += v
        }
        else {
            if(r.x1Pos && r.x2Pos) {
                qpObj.addTerm(coef, x1, x2)
            }
            else if(r.x1Pos && !r.x2Pos) {
                qpObj.addTerm(coef, x1)
                qpObj.addTerm(-coef, x1, x2)
            }
            else if(!r.x1Pos && r.x2Pos) {
                qpObj.addTerm(coef, x2)
                qpObj.addTerm(-coef, x1, x2)
            }
            else {
                qpObj.addTerm(-coef, x1)
                qpObj.addTerm(-coef, x2)
                qpObj.addTerm(coef, x1, x2)
            }
        }
    }

	/**
	 * generalizes `objective += coef * primary * max(secondary)`
	 */
    def quadTermWithMax(coef: Double, primary: IloIntVar, secondary: Seq[IloIntVar], r: Restriction = DefaultRestriction) {
		val mv = cplex.boolVar()
		for(v <- secondary) cplex.addGe(mv, v)
		newVariables += mv
		quadTerm(coef, primary, mv, r)
	}

}


