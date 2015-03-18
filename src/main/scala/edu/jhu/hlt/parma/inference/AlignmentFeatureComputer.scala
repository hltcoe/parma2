// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.inference

import edu.jhu.hlt.parma.types._

/**
 * not equivalent to edu.jhu.hlt.parma.AlignmentSimilarity,
 * which only represents one feature function
 * 
 * this is used to describe a group of features, like say HierarchicalAlignmentModule
 */
trait AlignmentFeatureComputer {
	def computeFeatures(a: Alignment, report: Document, passage: Document, domain: Option[String]): SVec
}

