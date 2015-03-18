// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import no.priv.garshol.duke.comparators.JaroWinkler
import java.util.Collection
import java.util.logging.Logger

class JaroWinklerSimilarityFeature extends AlignmentSimilarity {
  
	private[this] val binarizer = new FixedWidthBinarizer(12, false, 0d, 1d)

	private[this] val debug = false

	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {

		val (reportMention, passageMention) = CanonicalMentionFinder.canonicalMentions(a, report, passage)
		val s1 = report.getMentionString(reportMention)
		val s2 = passage.getMentionString(passageMention)

		//Sorted Winkler per Christen (2006)
		val s1p = normalizeAndTokenize(s1).sorted.mkString(" ")
		val s2p = normalizeAndTokenize(s2).sorted.mkString(" ")

		val jwd = JaroWinkler.similarity(s1, s2)
		val jwd_norm = JaroWinkler.similarity(s1p, s2p)

		if(debug) {
			println("report mention = " + Describe.mentionInContext(reportMention, report))
			println("passage mention = " + Describe.mentionInContext(passageMention, passage))
			println("jwd = " + jwd)
			println("jwd-norm = " + jwd_norm)
		}

		b(sv, jwd, "name-jaro-winkler", binarizer)
		b(sv, jwd_norm, "name-jaro-winkler-norm", binarizer)
	}

	def normalizeAndTokenize(s : String) : Array[String] = s.split(" ")
}
