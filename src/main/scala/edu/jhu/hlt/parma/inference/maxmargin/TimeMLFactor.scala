package edu.jhu.hlt.parma.inference.maxmargin

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.inference.temporal._
import ilog.concert._
import ilog.cplex._

class TimeMLFactor(
		val first: CPAlignment[PredicateAlignment],
		val second: CPAlignment[PredicateAlignment],
		val diag1: CPAlignment[PredicateAlignment],
		val diag2: CPAlignment[PredicateAlignment],
		val reportRel: TimeMLRelation,
		val passageRel: TimeMLRelation,
		val param: ParamRef)
		extends RelaxableCPFactor with HasSignature with Logging2 {
	
	val tightRelaxation = false

	// if we see a quadratic variable with a coefficient
	// that is smaller than this, then prune it for efficiency
	val pTooSmallToCare = ParmaConfig.getDouble("inference.ssvm.temporal.too-small-to-care")
	def threshold(v: Double, cutoff: Double): Double = {
		require(v >= 0d && cutoff >= 0d)
		if(v > cutoff) v else 0d
	}

	def pSameRelation(r1: TimeMLRelation, r2: TimeMLRelation): Double = {
		// this ignores the probability that these two relations
		// are the same by chance, so this is an underestimate.
		// computing the true odds would ruin sparsity, so i'm putting
		// it off for now
		if(r1.getClass == r2.getClass) {
			val p1 = r1.confidence
			val p2 = r2.confidence
			assert(p1 >= 0d && p1 <= 1d)
			assert(p2 >= 0d && p2 <= 1d)
			// geom avg instead of prod, keeps params consistent with a [0,1] feature
			math.sqrt(p1 * p2)
		}
		else 0d
	}

	def pInvRelation(r1: TimeMLRelation, r2: TimeMLRelation): Double = {
		// same note as pSameRelation about true probs and sparsity
		if(r1.getClass == r2.inverse.getClass) {
			val p1 = r1.confidence
			val p2 = r2.confidence
			assert(p1 >= 0d && p1 <= 1d)
			assert(p2 >= 0d && p2 <= 1d)
			math.sqrt(p1 * p2)
		}
		else 0d
	}

	val pFS: Double = threshold(pSameRelation(reportRel, passageRel), pTooSmallToCare)
	val pDD: Double = threshold(pInvRelation(reportRel, passageRel), pTooSmallToCare)
	lazy val fsGold: Double = if(first.isAligned && second.isAligned) 1d else 0d	// stay lazy, might not have label
	lazy val ddGold: Double = if(diag1.isAligned && diag2.isAligned) 1d else 0d
	var fsVar: IloIntVar = null
	var ddVar: IloIntVar = null
	var cplex: IloCplex = null

	require(pFS >= 0d && pFS <= 1d, "pFS = " + pFS)
	require(pDD >= 0d && pFS <= 1d, "pDD = " + pDD)

	override def relax(cplex: IloCplex) {
		require(!tightRelaxation)
		cplex.add(cplex.conversion(fsVar, IloNumVarType.Float))
		cplex.add(cplex.conversion(ddVar, IloNumVarType.Float))
	}

	def fsVarStr(gold: Boolean): String =
		varStr(gold, first, second)
	def ddVarStr(gold: Boolean): String =
		varStr(gold, diag1, diag2)
	private def varStr(gold: Boolean, a: CPAlignment[PredicateAlignment], b: CPAlignment[PredicateAlignment]): String = {
		if(gold)
			if(a.hasLabel)
				if(a.isAligned && b.isAligned) "+" else "-"
			else "?"
		else
			if(a.decode && b.decode) "+" else "-"
	}

	override def toString: String =
		"(TimeMLFactor param.value=%.3ff pFS=%.3f pDD=%.3f fsVar=[%s:%s] ddVar=[%s:%s])"
			.format(param.value, pFS, pDD, fsVarStr(true), fsVarStr(false), ddVarStr(true), ddVarStr(false))

	/**
	 * use this for selecting factors that will actually fire
	 * (higher means more likely to fire)
	 */
	def confidence: Double = pFS + pDD

	def willFire: Boolean = pFS > 0d || pDD > 0d

	override def register(cpObj: CPObjective) {
		//log("[TimeMLFactor pFS=%.2f pDD=%.2f]".format(pFS, pDD))
		cplex = cpObj.cplex

		fsVar = cplex.boolVar()
		if(tightRelaxation)
			cplex.addGe(fsVar, cplex.prod(first.getCplexVar, second.getCplexVar))
		else
			cplex.addGe(fsVar, cplex.sum(-1, cplex.sum(first.getCplexVar, second.getCplexVar)))
		cpObj.linearTerm(pFS * param.value, fsVar)

		ddVar = cplex.boolVar()
		if(tightRelaxation)
			cplex.addGe(ddVar, cplex.prod(diag1.getCplexVar, diag2.getCplexVar))
		else
			cplex.addGe(ddVar, cplex.sum(-1, cplex.sum(diag1.getCplexVar, diag2.getCplexVar)))
		cpObj.linearTerm(pDD * param.value, ddVar)
	}

	def fsHyp = cplex.getValue(fsVar)
	def ddHyp = cplex.getValue(ddVar)

	// theta    = param.value
	// features = pFS   + pDD
	// coef		= 1
	// variable = fsVar + ddVar

	override def accumGradient(gradBuf: DVec) {
		gradBuf(param.index) += pFS * (fsGold - fsHyp)
		gradBuf(param.index) += pDD * (ddGold - ddHyp)
	}

	override def getState(thetaDim: Int): CPFactorState = {
		val g = DVec.rep(0d, thetaDim)
		val h = DVec.rep(0d, thetaDim)
		if(fsVar != null) {
			g(param.index) += pFS * fsGold
			h(param.index) += pFS * fsHyp
		}
		if(ddVar != null) {
			g(param.index) += pDD * ddGold
			h(param.index) += pDD * ddHyp
		}
		val costs = Array(pFS * param.value, pDD * param.value)
		val vars = Array(fsHyp, ddHyp)
		CPFactorState(this, g, h, costs, vars, 0d)
	}

	override def signature: Int = 0
}

