// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.math

import scala.collection.JavaConversions._

import no.uib.cipr.matrix.VectorEntry
import no.uib.cipr.matrix.sparse.SparseVector

object Stats {

	val LOG_E_2 = Math.log(2.0)

	def mean(sd: Traversable[Double], skipNaN: Boolean = false): Double = {
		var s = 0d
		var n = 0
		for(d <- sd) {
			if(d.isNaN) {
				if(!skipNaN) throw new RuntimeException("got a NaN")
			} else {
				s += d
				n += 1
			}
		}
		assert(n > 0)
		s / n
	}

	def variance(sd: Traversable[Double], skipNaN: Boolean = false): Double = {
		var sx = 0d
		var sxx = 0d
		var n = 0
		for(d <- sd) {
			if(d.isNaN) {
				if(!skipNaN) throw new RuntimeException("got a NaN")
			} else {
				sx += d
				sxx += d*d
				n += 1
			}
		}
		assert(n > 0)
		val ex = sx/n
		(sxx/n) - ex*ex
	}

	def stdDev(sd: Traversable[Double], skipNaN: Boolean = false): Double = math.sqrt(variance(sd, skipNaN))

	def order(p: Double, skipNaN: Boolean = false): Double = {
		throw new RuntimeException("implement me")
	}

	def vsum(v: SparseVector): Double = {
		var sum = 0.0
		for (value <- v.getData())
			sum += value
		return sum;
	}

	def klDivergence(from: SparseVector, to: SparseVector): Double = {
		val fromTot = vsum(from)
		val toTot = vsum(to)

		var result = 0.0
		for (vEntry <- from) {
			var fromVal = vEntry.get()

			if (fromVal > 0.0) {
				fromVal /= fromTot
				var toVal = to.get(vEntry.index())
				toVal /= toTot

				var logFract = Math.log(fromVal / toVal)

				if (logFract == Double.NegativeInfinity)
					return Double.NegativeInfinity

				result += fromVal * (logFract / LOG_E_2) // express it in log base 2
			}
		}

		return result;
	}

	def JSDistance(v1: SparseVector, v2: SparseVector): Double = {
		var m = new SparseVector(v1, true)
		m.scale(1.0 / vsum(v1))
		m.add(v2.copy().scale(1.0 / vsum(v2)))
		m.scale(0.5)

		var kl1 = klDivergence(v1, m)
		var kl2 = klDivergence(v2, m)

		if (kl1 == Double.NegativeInfinity || kl1 == Double.NaN ||
			kl2 == Double.NegativeInfinity || kl2 == Double.NaN) {
			throw new RuntimeException(String.format("%s: Negative infinity encountered%n%s%n%s", this.getClass().getName(), v1.toString(), v2.toString()));
		}

		// The KL divergence can get very small. Clamp it to zero if it is below a threshold.
		kl1 = if (Math.abs(kl1) < 1.0e-14) 0.0 else kl1
		kl2 = if (Math.abs(kl2) < 1.0e-14) 0.0 else kl2

		// See Endres and Schindelin (2003) for why we use this formula for the final computation.
		var jsd = Math.sqrt(kl1 + kl2)

		// Sanity check
		if (jsd == Double.NaN) {
			System.err.println(kl1)
			System.err.println(kl2)
			throw new RuntimeException("JSD is NaN!")
		}

		return jsd
	}

}
