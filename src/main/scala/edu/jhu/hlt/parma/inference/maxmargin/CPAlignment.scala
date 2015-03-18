package edu.jhu.hlt.parma.inference.maxmargin

import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.inference.DocMetaAligner
import ilog.concert._
import ilog.cplex._
import java.util.Arrays
import collection.mutable.ArrayBuffer

/**
 * represents an alignment in a grid includes a unary factor
 */
class CPAlignment[T <: Alignment](
        val alignment: T,
		private val aligned: Boolean,	// doesn't matter if hasLabel=false
		val hasLabel: Boolean,
        val features: SVec,
        val row: Int, val col: Int)
		extends RelaxableCPFactor with HasSignature with Logging2 {

	val verbose = false
	
	def this(a: T, ia: Boolean, f: SVec, r: Int, c: Int) =	// with label
		this(a, ia, true, f, r, c)
	def this(a: T, f: SVec, r: Int, c: Int) =				// without label
		this(a, false, false, f, r, c)
	def this(a: T, ia: Option[Boolean], f: SVec, r: Int, c: Int) =
		this(a, ia.getOrElse(false), !ia.isEmpty, f, r, c)

    var cplex: IloCplex = null
    var cplexVar: IloIntVar = null
	var sim = -99999d

	override def toString: String = {
		val l = if(hasLabel) {
			if(aligned) "+" else "-"
		} else "?"
		val d = if(cplex != null) {
			if(decode) "+"
			else "-"
		} else "?"
		"(CPAlignment @(%d,%d) lab=%s decode=%s sim=%.4g a=%s)".format(
			row, col, l, d, sim, alignment)
	}

	override def relax(cplex: IloCplex) {
		require(cplexVar != null)
		val conv = cplex.conversion(cplexVar, IloNumVarType.Float)
		cplex.add(conv)
	}

	def isAligned: Boolean = {
		if(!hasLabel)
			throw new RuntimeException("this has no label!")
		aligned
	}

	/** useful for concise print statements */
	def isAlignedStr: String = {
		if(hasLabel) {
			if(isAligned) "+"
			else "-"
		}
		else "?"
	}

	def getCplexVar: IloIntVar = {
		if(cplexVar == null)
			throw new RuntimeException
		cplexVar
	}

	/**
	 * compute similarity from stored features and according
	 * to passed in parameter vector.
	 * if you want to mimic parma1 style thresholding, you can
	 * add an offset which is added to the similarity score like
	 * another intercept feature.
	 */
	def updateSimilarity(theta: DVec, offset: Double = 0d) {
		sim = VecOps.dot(theta, features) + offset
	}

    override def register(cpObj: CPObjective) {
		logIf(verbose, "[CPAlignment register] (%d,%d) sim=%.2f".format(row, col, sim))
        cplex = cpObj.cplex
        cplexVar = cplex.boolVar
		cpObj.linearTerm(sim, cplexVar)
    }

	/**
	 * did cplex say this was a true alignment?
	 */
    def decode: Boolean = {
        if(cplex == null || cplexVar == null)
            throw new RuntimeException("you must register first")
        val v = cplex.getValue(cplexVar)
        v >= 0.5d
    }

	def accumGradient(gradBuf: DVec) {
		if(!hasLabel) {
			warn("not computing gradient on an unlabeled example")
			return
		}
		implicit def b2d(b: Boolean): Double = if(b) 1d else 0d
		val delta: Double = isAligned - decode
		if(verbose) {
			val d = SVec.duplicate(features)
			d *= delta
			log("[CPAlignment accumGrad] (%d,%d) %sgold %shyp delta=%.2f partialGrad=%s"
				.format(row, col, if(isAligned)"+"else"-", if(decode)"+"else"-", delta, Describe.svec(d).substring(0,20) + "..."))
		}
		if(delta != 0d)
			VecOps.add(gradBuf, features, delta)
	}

	val hash = util.Random.nextInt
	override def signature = if(decode) hash else 0//~hash

	override def getState(thetaDim: Int): CPFactorState = {
		val g = DVec.rep(0d, thetaDim)
		if(isAligned) g += features
		val h = DVec.rep(0d, thetaDim)
		if(decode) h += features
		val costs = Array(sim)
		val vars = Array(cplex.getValue(cplexVar))
		CPFactorState(this, g, h, costs, vars, 0d)
	}
}

