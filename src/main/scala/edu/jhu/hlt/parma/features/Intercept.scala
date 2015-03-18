// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.feature_interfaces.AlignmentSimilarity

/**
 * single-dimensional feature that always fires
 *
 * implementation note: don't make this an object or it makes
 * it difficult to load using reflection
 */
class Intercept extends AlignmentSimilarity {
	override def featureName(idx: Int): String = {
		assert(idx == 0, "idx = " + idx)
		"intercept"
	}
	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {
		sv.add(0, 1d)
	}
}

