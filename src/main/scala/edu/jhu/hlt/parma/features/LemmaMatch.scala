// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder
import edu.jhu.hlt.parma.types._

/**
 * used as a baseline in
@InProceedings{roth-frank:2012:STARSEM-SEMEVAL,
  author    = {Roth, Michael  and  Frank, Anette},
  title     = {Aligning Predicate Argument Structures in Monolingual Comparable Texts: A New Corpus for a New Task},
  booktitle = {{*SEM 2012}: The First Joint Conference on Lexical and Computational Semantics -- Volume 1: Proceedings of the main conference and the shared task, and Volume 2: Proceedings of the Sixth International Workshop on Semantic Evaluation {(SemEval 2012)}},
  month     = {7-8 June},
  year      = {2012},
  address   = {Montr\'{e}al, Canada},
  publisher = {Association for Computational Linguistics},
  pages     = {218--227},
  url       = {http://www.aclweb.org/anthology/S12-1030}
}
 */
class LemmaMatch extends BinaryAlignmentSimilarity {
	override def fires(a: Alignment, report: Document, passage: Document): Boolean = {
		// can use head token for Roth and Frank data because
		// spans should be just one word
		// this generalizes to Stanford's EECB more gracefully
		// where there are phrases and the head token must be chosen more carefully
		val (reportMention, passageMention) = CanonicalMentionFinder.canonicalMentions(a, report, passage)
		val rh = report.getHeadToken(reportMention)
		val ph = passage.getHeadToken(passageMention)
		rh.getLemma equalsIgnoreCase ph.getLemma
	}
}

