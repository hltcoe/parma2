// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.feature_interfaces.AlignmentSimilarity
import util.Random

/**
 * for debugging
 */
class RandomFeature extends AlignmentSimilarity {

	val rand = new Random(9001)
	val howMany = 5
	val sparsity = 0

	val indices: Set[Int] = (0 until howMany).map(_ * (1+sparsity)).toSet

	override def featureName(idx: Int): String = {
		require(indices.contains(idx))
		"random" + idx
	}
	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {
		for(i <- indices) sv.add(i, rand.nextGaussian)
	}
}

