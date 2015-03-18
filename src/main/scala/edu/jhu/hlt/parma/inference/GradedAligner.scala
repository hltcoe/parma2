// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.inference

import edu.jhu.hlt.parma.util.ParmaConfig

class GradedAligner {

	private var possibleThresh = ParmaConfig.getDouble("inference.threshold.possible", 0.2)
	private var sureThresh = ParmaConfig.getDouble("inference.threshold.sure", 0.3)
	
	def possibleThreshold = {
		assert(sureThresh >= possibleThresh)
		possibleThresh
	}
	
	def setPossibleThreshold(v: Double) {
		assert(v <= sureThresh)
		possibleThresh = v
	}
	
	def sureThreshold = {
		assert(sureThresh >= possibleThresh)
		sureThresh
	}
	
	def setSureThreshold(v: Double) {
		assert(v >= possibleThresh)
		sureThresh = v
	}
	
	def setThresholds(sure: Double, possible: Double) {
		assert(sure >= possible)
		sureThresh = sure
		possibleThresh = possible
	}
	
}

