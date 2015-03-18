package edu.jhu.hlt.parma.inference.maxmargin

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._

/**
 * TODO
 * for debugging, i want to know what was aligned in a constraint
 * only seeing delta features and loss is not enough
 *
 * once i know which alignments differ, i can narrow down 
 *
 * ideally i would just know what deltefeatures and loss was for each
 * call to accumGradient
 * i would compare this to the costs that cplex saw
 *
 * cplex is solving something other that what is reflected in the constraint
 * constraint score = hinge = f(delta features, score)
 * cplex score      = LP = weights on every feature
 *
 * tests:
 * 1. should be able to make cplex score and constraint score match!
 *    data needed:
 *      cplex:      Map[CPFactor, (Double, Boolean)]
 *      constraint: Map[CPFactor, DVec], Loss
 * 2. gradBuf/deltaFeatures should be the sum of the parts for every instance
 */
class CplexScoreDebug(val theta: DVec, val cpg: CPGlobalAlignment) extends Logging2 {
	// need to find out why cplex doesn't find the the constraint with the highest hinge
	// need to take something apart to find the bug
	// take apart cplex score:
		// explicitly track the ILP vars and costs
	// take apart constraint hinge:
		// track accumGrad calls


	// the unit on both of these is a CPFactor
	// hinge = max(0, sum_{factors} deltaFeatures(factor) + loss(factor))
	// ilp = sum_{factors} costs(factor) * vars(factor)

	// ILP = max w * f(z) + loss(z)
	// SVM = max w * [f(z) - f(~z)] + loss(~z)

	// CPFactorState.deltaFeatures should actually be f(~z) or f(z)
	// -- or -- i could include f(z) in the state

	def checkEqual(a: Double, b: Double, msg: String = null, warnTolerance: Double = 1e-5, crashTolerance: Double = 1e-3) {
		val d = math.abs(a - b)
		if(d > warnTolerance) {
			println(">>>>> WARNING: a=%f b=%f".format(a, b))
			if(msg != null)
				println(msg)
		}
		require(d < crashTolerance)
	}

	def verifyScores(justBuilt: Constraint, cplexObjective: Double) {
		log("[verifyScores] cons.hinge=%f cplexObjective=%f"
			.format(justBuilt.hinge(theta), cplexObjective))
		var hypFeatures = DVec.rep(0d, theta.dimension)
		var x_cplex = 0d
		var x_cons = new Constraint(theta.dimension)
		for(f <- cpg.allFactorsIncludingLoss) {
			val s: CPFactorState = f.getState(theta.dimension)
			x_cplex += s.ilpScore
			x_cons.addAlignment(s.deltaFeatures, s.loss, 0)
			hypFeatures += s.hypFeatures
			log("[verifyScores] after %s s.loss=%f s.ilpScore=%f score=%f".format(f, s.loss, s.ilpScore, x_cplex))
			checkEqual(VecOps.dot(theta, s.hypFeatures) + s.loss, s.ilpScore)
		}

		checkEqual(cplexObjective, x_cplex, msg="cplexObj=%f x_cplex=%f".format(cplexObjective, x_cplex))

		val hypScore = VecOps.dot(hypFeatures, theta)
		checkEqual(cplexObjective, hypScore + x_cons.getLoss,
			msg="cplexObj=%f =/= hypScore=%f + loss=%f = %f"
				.format(cplexObjective, hypScore, x_cons.getLoss, hypScore + x_cons.getLoss))

		val dScore = VecOps.dot(x_cons.getDeltaFeatures, theta)
		checkEqual(justBuilt.hinge(theta), math.max(0d, x_cons.getLoss - dScore),
			msg="justBuilt.hinge=%f =/= max(0, loss=%f - dScore=%f) = %f"
				.format(justBuilt.hinge(theta), x_cons.getLoss, dScore, math.max(0d, x_cons.getLoss - dScore)))

		checkEqual(justBuilt.getLoss, x_cons.getLoss)

		for(i <- 0 until theta.dimension)
			checkEqual(justBuilt.getDeltaFeatures(i), x_cons.getDeltaFeatures(i))

		log("[verifyScores] this one looks good\n")
	}
}

case class CPFactorState(val factor: CPFactor, val goldFeatures: DVec, val hypFeatures: DVec,
		val costs: Array[Double], val variables: Array[Double], val loss: Double) {

	require(costs.size == variables.size)
	require(goldFeatures.dimension == hypFeatures.dimension)
	//warnIf(variables.any(v => v!=1d || v!=0d), "non-integer solutions!")

	costs.foreach(c => {
		require(!c.isInfinite)
		require(!c.isNaN)
	})
	variables.foreach(v => {
		require(!v.isInfinite)
		require(!v.isNaN)
	})
	
	require(!loss.isInfinite)
	require(!loss.isNaN)

	require(!goldFeatures.containsBadValues(checkForNaN=true, checkForInf=true))
	require(!hypFeatures.containsBadValues(checkForNaN=true, checkForInf=true))

	val ilpScore: Double = {
		var i = 0
		var s = 0d
		while(i < costs.size) {
			s += costs(i) * variables(i)
			i += 1
		}
		s
	}

	lazy val deltaFeatures: DVec = goldFeatures - hypFeatures
}

