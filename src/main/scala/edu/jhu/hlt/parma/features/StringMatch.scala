// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder

/**
 * this class is useful for baseline experiments
 */
class StringMatch extends BinaryAlignmentSimilarity {
	override def fires(a: Alignment, report: Document, passage: Document): Boolean = {
		val (reportMention, passageMention) = CanonicalMentionFinder.canonicalMentions(a, report, passage)
		val rHt = report.getHeadToken(reportMention)
		val pHt = passage.getHeadToken(passageMention)
		rHt.getWord equalsIgnoreCase pHt.getWord
	}
}

