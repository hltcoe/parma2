// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.evaluation

import edu.jhu.hlt.parma.types.{ DocAlignment, Instance }

/**
 * http://acl.ldc.upenn.edu/J/J03/J03-1002.pdf
 */
object AlignmentErrorRate {
	def aer(inst: Instance[DocAlignment]): Double = {
		val s = inst.gold.sureAlignments
		val p = inst.gold.possibleAlignments
		val a = inst.hyp.sureAlignments
		assert(inst.hyp.possibleAlignments.size == inst.hyp.sureAlignments.size)
		1d - ((a & s).size + (a & p).size) / (a.size + s.size)
	}
}

