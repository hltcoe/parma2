// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.evaluation

class PrecisionRecallF1(val truePositives: Double, val numHyp: Double, val numGold: Double) {

	def this(tp: Int, nh: Int, ng: Int) = this(tp.toDouble, nh.toDouble, ng.toDouble)

	val precision: Double = {
		if(numHyp == 0d) 1d
		else truePositives / numHyp
	}

	val recall: Double = {
		if(numGold == 0d) 1d
		else truePositives / numGold
	}

	val f1: Double = {
		if(recall + precision == 0d) 0d
		else 2d * precision * recall / (precision + recall)
	}

	/**
	 * WARNING: this assumes that there is no overlap in the
	 * hypothesis or gold space between examples! only use in
	 * cases where the precision/recall/f1 from one instance
	 * has nothing to do with that of the other.
	 */
	def +(other: PrecisionRecallF1): PrecisionRecallF1 = {
		new PrecisionRecallF1(truePositives + other.truePositives,
			numHyp + other.numHyp, numGold + other.numGold)
	}
	
	override def toString =
		"precision = %.2f\trecall = %.2f\tF1 = %.2f".format(precision, recall, f1)
}

