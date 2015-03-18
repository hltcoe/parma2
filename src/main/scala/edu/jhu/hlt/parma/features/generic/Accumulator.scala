// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features.generic

import edu.jhu.hlt.parma.types.DVec

/**
 * POINTWISE accumulators
 */
class Accumulator(name: String, val accumulate: Seq[Double] => Double)
	extends Pipe[Seq[DVec], DVec](name, (sd: Seq[DVec]) => {
		if(sd.size == 0) {
			//throw new RuntimeException("empty features for " + name)
			throw new RuntimeException("this does not jive with addStable()/ffWidth")
			DVec.zero1
		}
		else {
			val a = DVec.zero(sd.head.dimension)
			for(dv <- sd) a += dv
			a
		}
	}) {

	/**
	 * returns a similarity measure where every index in the returned
	 * DVec is multiplied by scale and then added to shift
	 */
	def linearTransform(scale: Double, shift: Double): Accumulator = {
		val newName = name + "$scaled"
		val newFunc = (sd: Seq[Double]) => accumulate(sd)*scale + shift
		new Accumulator(newName, newFunc)
	}
}

case class IdentAccumMaybeEmpty(val outputDim: Int) extends Pipe[Seq[DVec], DVec]("ident", (sd: Seq[DVec]) => {
	if(sd.size == 0) {
		// TODO switch to an agnostic value, like in Binarizer
		// DVecs are now mutable, don't just store one
		DVec.zero(outputDim)
	}
	else if(sd.size == 1) {
		val dv = sd.head
		assert(dv.dimension == outputDim)
		dv
	}
	else throw new RuntimeException("IdentAccumMaybeEmpty must receive 1 or 0 DVecs! sd=" + sd)
})

object IdentAccum extends Pipe[Seq[DVec], DVec]("ident", (sd: Seq[DVec]) => {
	if(sd.size == 0)
		throw new RuntimeException("if you think that you might get an empty seq by the end of the pipe, use IdentAccumMaybeEmpty")
	else if(sd.size == 1)
		sd.head
	else throw new RuntimeException("IdentAccum must receive exactly 1 DVec! sd=" + sd)
})

object AccumulatorImplementation {

	// note that all of these will throw an exception when given an empty seq
	val sum = new Accumulator("sum", (sd: Seq[Double]) => sd.sum)
	val avg = new Accumulator("avg", (sd: Seq[Double]) => sd.sum / sd.size.toDouble)
	val max = new Accumulator("max", (sd: Seq[Double]) => sd.max)
	val min = new Accumulator("min", (sd: Seq[Double]) => sd.min)

	private val sumAvgAlpha = 0.7d	// 1=>sum, 0=>avg
	val sumAvg = new Accumulator("sumAvg", (sd: Seq[Double]) => sd.sum / (sumAvgAlpha + (1d - sumAvgAlpha)*sd.size))
}

