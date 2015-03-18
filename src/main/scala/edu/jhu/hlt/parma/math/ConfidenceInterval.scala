// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.math

class ConfidenceInterval(val numbers: Seq[Double]) {

	val infinity = 1d/0d
	assert(numbers.size > 0)
	
	val mean = {
		val n = numbers.sum
		n / numbers.size
	}
	
	val variance = {
		if(numbers.size == 1) infinity
		else {
			var (sx, sxx) = (0d, 0d)
			numbers.foreach(x => { sx += x; sxx += x*x })
			val ex = sx / numbers.size
			val exx = sxx / numbers.size
			val variance = exx - ex*ex
			if(variance < 1e-12) {
				println("[ConfidenceInterval] WARNING: variance = %g, numbers = %s"
					.format(variance, numbers))
				//assert(false)
			}
			variance
		}
	}
	
	val stderr = math.sqrt(variance)
	
	def interval(z: Double = 1.96) = {
		val mu = mean
		val pad = z * stderr / math.sqrt(numbers.size)
		(mu - pad, mu + pad)
	}
	
	val (lo, hi) = interval()

	override def toString = "(mu=%.3f, lo=%.3f, hi=%.3f)".format(mean, lo, hi)
	
}

