// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder
import edu.jhu.hlt.parma.util.Misc

class CountFeatures extends AlignmentSimilarity {

	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {

		val pred = a.isInstanceOf[PredicateAlignment]

		// addition prevents divide by 0, adds smoothing
		val (rs, ps) =
			if(pred) (report.predicates.size + 1, passage.predicates.size + 1)
			else (report.arguments.size + 1, passage.arguments.size + 1)
		val (rn, pn) =
			if(pred) (report.predicates.size + 2, passage.predicates.size + 2)
			else (report.arguments.size + 2, passage.arguments.size + 2)
		val rd = rs.toDouble / rn
		val pd = ps.toDouble / pn

		featureIndexer.start(sv)

		// absolute number of preds/args
		featureIndexer.addStable("same#PAS", bool2value(rs == ps))
		featureIndexer.addStable("diff#PAS", math.abs(rs - ps) / 20d)
		featureIndexer.addStable("sum#PAS", (rs + ps) / 2000d)
		featureIndexer.addStable("min#PAS", math.min(rs, ps) / 200d)
		featureIndexer.addStable("max#PAS", math.max(rs, ps) / 1000d)

		// density of preds/args
		featureIndexer.addStable("sameDensityPAS", bool2value(math.abs(rd - pd) < 0.2d))
		featureIndexer.addStable("diffDensityPAS", math.abs(rd - pd))
		featureIndexer.addStable("sumDensityPAS", rd + pd)
		featureIndexer.addStable("minDensityPAS", math.min(rd, pd))
		featureIndexer.addStable("maxDensityPAS", math.max(rd, pd))

		featureIndexer.commit
	}

}


