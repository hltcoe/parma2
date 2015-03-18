// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.math

object Functions {
	def logAdd(x: Double, y: Double): Double = {
		if(x == java.lang.Double.NEGATIVE_INFINITY) y
		else if(y == java.lang.Double.NEGATIVE_INFINITY) x
		else math.max(x, y) + math.log1p(math.exp( -math.abs(x - y) ))
	}
}

