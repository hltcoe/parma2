package edu.jhu.hlt.parma.inference.maxmargin

import edu.jhu.hlt.parma.types.DVec
import edu.jhu.hlt.parma.util.Logging2
import collection.mutable.ArrayBuffer
import java.util.BitSet

class WorkingSet(val checkSignatures: Boolean = false) extends Iterable[Constraint] with Logging2 {

	private[this] val constraints = new ArrayBuffer[Constraint]
	private[this] val gaurd = new BitSet

	var verbose = false

	/**
	 * returns true if c was not already in the set
	 */
	def add(c: Constraint): Boolean = {
		val sig = c.unsignedSignature
		if(!checkSignatures || !gaurd.get(sig)) {
			gaurd.set(sig)
			constraints += c
			true
		}
		else false
	}

	override def size: Int = constraints.size

	override def iterator: Iterator[Constraint] = constraints.toSeq.iterator

	def clear {
		constraints.clear
		gaurd.clear
	}

	/**
	 * returns the most violated constraint and it's hinge
	 */
	def mostViolated(theta: DVec): (Constraint, Double) = {
		require(constraints.size > 0, "working set is empty, there is no most violated constraint!")
		val (c, h) = constraints.map(c => (c, c.hinge(theta))).maxBy(_._2)
		logIf(verbose, "[WorkingSet mostViolated] constraint=%d\thinge=%.2g".format(c.unsignedSignature, h))
		(c, h)
	}

	// TODO add shrinkage heuristics here
}

