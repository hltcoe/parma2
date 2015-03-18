package edu.jhu.hlt.parma.inference.maxmargin

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._

object Constraint {
	def sig2string(sig: Int): String =
		Integer.toHexString(sig).toUpperCase
}

/**
 * defines a 1-slack constraint for algorithm 3 in
 * http://www.cs.cornell.edu/people/tj/publications/joachims_etal_09a.pdf
 *
 * weights are useful in trading off between optimizing macro and micro versions
 * of your loss funcion. mathematically, this constraint encodes
 *   xi + score(z) * weight / N >= (score(\hat{z}) + loss(z, \hat{z})) * weight / N
 * where N = size of this constraint (how many DocAlignments/deltaFeatures/losses are included)
 * Assuming score(z) and loss(z, \hat{z}) \propto size(z),
 *   weight = 1 :: this is regular (micro-oriented) svm constraint
 *   weight = E[size(z)] / size(z) :: this is macro-oriented objective
 */
class Constraint(val dimension: Int) extends Logging2 {

	private val deltaFeatures = DVec.zero(dimension)	// \sum_i \mu_i [f(z) - f(\hat{z})]
	private var loss = 0d								// \sum_i \mu_i loss(\hat{z})
	private var sig = 0									// approx equals/hashcode
	
	val verbose = false

	def *=(factor: Double) {
		deltaFeatures *= factor
		loss *= factor
	}

	def getLoss: Double = loss
	def getDeltaFeatures: DVec = deltaFeatures.copy
	def getSig: Int = sig

	/**
	 * as if all calls to c.addAlignment had happened to this constraint
	 */
	def +=(c: Constraint) {
		require(dimension == c.dimension)
		deltaFeatures += c.deltaFeatures
		loss += c.loss
		sig ^= c.sig
		require(!containsBadValues())
	}

	def +(c: Constraint): Constraint = {
		val r = new Constraint(dimension)
		r += this
		r += c
		r
	}

	def getSigString: String = Constraint.sig2string(signature)

	override def toString: String =
		"(Constraint sig=%s df.l2=%.1f loss=%.1f)".format(getSigString, deltaFeatures.l2, loss)

	/**
	 * deltaFeatures = f(z) - f(\hat{z}), used for gradient and other things
	 * loss = \Delta(\hat{z}) (NOTE: need not be non-negative!)
	 * signature should be a hashcode for \hat{z}
	 *
	 * default weight=1 will optimize micro loss (assuming deltaFeatures, loss \propto size)
	 * setting weight=E[size(alignment)]/size(alignment) will optimize macro loss
	 */
	def addAlignment(deltaFeatures: DVec, loss: Double, signature: Int, weight: Double = 1d) {
		require(deltaFeatures.dimension == this.deltaFeatures.dimension)
		require(weight > 0d)
		VecOps.add(this.deltaFeatures, deltaFeatures, weight)
		this.loss += weight * loss
		this.sig ^= signature
		if(verbose) {
			log("[addAlignment] deltaFeatures.l2=%.2f deltaFeatures.l1=%.2f loss=%.2f weight=%.2f signature=%s"
				.format(deltaFeatures.l2, deltaFeatures.l1, loss, weight, Constraint.sig2string(signature)))
		}
		require(!containsBadValues())
	}

	def isViolated(theta: DVec): Boolean = hinge(theta) > 0d

	/**
	 * NOTE: only call this when this (1-slack) constraint is FULLY BUILT
	 * do not use this for building a fully built constraint, namely you should
	 * never do:
	 *   val pc = mvCache.maxBy(score + loss)	// pc="partial constraint"
	 *   fullConstraint.addAlignment(pc.deltaFeatures, pc.hinge, pc.sig)
	 */
	def hinge(theta: DVec): Double = {
		val tdf = VecOps.dot(theta, deltaFeatures)	// theta * f(z) - theta * f(\hat{z})
		math.max(0d, loss - tdf)
	}

	def accumGradient(theta: DVec, grad: DVec) {
		if(hinge(theta) > 0d)
			VecOps.add(grad, this.deltaFeatures, 1d)
	}
	
	/**
	 * we don't want to add duplicate constraints, but i don't
	 * really want to keep around pointers to all the alignments
	 * that this constraint touched, so just use a hash.
	 * with 64 bits, there's not much chance of collision
	 */
	override def equals(other: Any): Boolean = {
		if(other.isInstanceOf[Constraint])
			signature == other.asInstanceOf[Constraint].signature
		else false
	}
	override def hashCode: Int = sig

	/**
	 * note that we do not use a salt here because
	 * we want constraints to be able to collide.
	 * if we salted, constraints would not collide
	 * even when all their alignments are the same
	 */
	def signature: Int = sig

	/**
	 * use this if youa are using a java.util.BitSet and need indices >= 0
	 */
	def unsignedSignature: Int = if(sig < 0) ~sig else sig

	def containsBadValues(checkForNaN: Boolean = true, checkForInf: Boolean = true): Boolean = {
		def bad(d: Double) = (checkForNaN && d.isNaN) || (checkForInf && d.isInfinite)
		val allGood = deltaFeatures.values.forall(d => !bad(d))
		if(!allGood) println("df is bad")
		if(bad(loss)) println("loss = " + loss)
		!allGood || bad(loss)
	}
}


