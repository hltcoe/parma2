// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.feature_interfaces

import edu.jhu.hlt.parma.types._

/**
 * use this for features that produce *one* boolean value
 * (this is used in NoTrainAligner, requires only one boolean)
 */
trait BinaryAlignmentSimilarity extends AlignmentSimilarity {

	def fires(a: Alignment, report: Document, passage: Document): Boolean
	
	final def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {
		if(fires(a, report, passage))
			b(sv, "binary-feature")
	}
}

