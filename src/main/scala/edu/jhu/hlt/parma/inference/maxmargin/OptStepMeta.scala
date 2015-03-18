package edu.jhu.hlt.parma.inference.maxmargin

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.util.Snapshot.delta

// NOTE TO FUTURE SELF:
// this class was a terrible idea
// it is gawky and takes a lot of logic that should be in CPInferenceEngine

object OptStepMeta {

	val ewLambda = 0.1d
	val eqInit = 99999d

	def initial: OptStepMeta = {
		val now = System.currentTimeMillis
		new OptStepMeta(0,
			Snapshot(Double.NaN, 0d),
			Snapshot(Double.NaN, 0d),
			eqInit,
			false, false,
			0, None, None, 0,
			now, Snapshot(now, now)
		)
	}
}

class OptStepMeta(
		val iter: Int,					// 0-indexed
		val hinge: Snapshot[Double],	// aka "slack", := max_y loss(y) - theta * [ f(y_lab) - f(y) ], or 0 if no constraint was added
		val objective: Snapshot[Double],
		val objectiveExpAvg: Double,		// exponential weighted average up to and including objective.cur
		val addedCachedConstraint: Boolean,
		val addedCplexConstraint: Boolean,
		val workingSetSize: Int,
		val theta: Option[DVec],
		val gradient: Option[DVec],
		val qpOptIters: Int,
		val startTime: Long,		// when optimization started from System.currentTimeMillis
		val time: Snapshot[Long])
		extends Logging2 {	// start and end points from System.currentTimeMillis

	def this(prev: OptStepMeta, hinge: Double, objective: Double,
			addedCachedConstraint: Boolean, addedCplexConstraint: Boolean,
			workingSetSize: Int, qpOptIters: Int,
			theta: Option[DVec] = None, gradient: Option[DVec]) =
		this(prev.iter+1, Snapshot(prev.hinge.cur, hinge), Snapshot(prev.objective.cur, objective),
			math.max(prev.objectiveExpAvg * (1d-OptStepMeta.ewLambda) + objective * OptStepMeta.ewLambda, objective),
			addedCachedConstraint, addedCplexConstraint, workingSetSize, theta, gradient,
			qpOptIters, prev.startTime, Snapshot(prev.time.cur, System.currentTimeMillis - prev.time.cur))

	require(!(addedCachedConstraint && addedCplexConstraint))
	require(iter >= 0)
	require(qpOptIters >= 0)

	def addedAConstraint: Boolean = addedCachedConstraint || addedCplexConstraint
	def addedConsDesc: String =
		if(addedCachedConstraint) "cached"
		else if(addedCplexConstraint) "cplex"
		else "none"

	def duration: Double = delta(time) / 1000d
	def elapsed: Double = (time.cur - startTime) / 1000d

	def done(maxIter: Option[Int] = None, maxTimeInSeconds: Option[Double] = None): Boolean = {

		if(iter == 0) return false

		if(!addedAConstraint) {
			log("[OptStepMeta] done because we did not add a constraint: " + toString)
			return true
		}

		if(iter >= maxIter.getOrElse(iter+1))
			return true

		val el = elapsed
		if(el > maxTimeInSeconds.getOrElse(el+1d))
			return true

		return false
	}

	override def toString: String = {
		val sb = new StringBuilder
		sb.append("(Step ")
		sb.append("iter=" + iter + " ")
		sb.append("slack=%f ".format(hinge.cur))
		sb.append("objective=%f delta-obj=%f ".format(objective.cur, delta(objective)))
		sb.append("objectiveAvg=%f ".format(objectiveExpAvg))
		sb.append("added-constraint=" + addedConsDesc + " ")
		sb.append("working-set=" + workingSetSize + " ")
		def tg(desc: String, v: Option[DVec]) {
			v match {
				case None => {}
				case Some(t) =>
					sb.append("%s.l2=%f %s.lInf=%f ".format(desc, t.l2, desc, t.lInf))
			}
		}
		tg("theta", theta)
		tg("gradient", gradient)
		sb.append("elapsed=%.1f".format(elapsed))
		sb.append(")")
		sb.toString
	}
}

