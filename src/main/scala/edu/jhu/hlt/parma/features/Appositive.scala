// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._

class Appositive extends AlignmentSimilarity {
	
	val debug = false

	// if one or both of these mentions looks like an appositive, try to compare
	// both sides of the comman
	// e.g. "World's sexiest man, Hugh Jackman" and "Hugh Jackman, one sexy dude"
	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document)  {

		val hsBuf = new java.util.HashSet[String]

		val (rm, pm) = CanonicalMentionFinder.canonicalMentions(a, report, passage)
		val rtoks = report.getMentionTokens(rm)
		val ptoks = passage.getMentionTokens(pm)
		val rci = rtoks.indexWhere(_.getWord == ",")
		val pci = ptoks.indexWhere(_.getWord == ",")

		val rhead = report.getHeadToken(rm)
		val phead = passage.getHeadToken(pm)

		assert(rci != 0 && pci != 0, "why do you have a mention that starts with a comma?")
			
		// default values
		var wordJaccard = 0d
		var lemmaJaccard = 0d
		var nerJaccard = 0d

		var unigrams = 0
		var bigrams = 0
		var trigrams = 0

		var headWordMatch = false
		var headLemmaMatch = false

		if(rci > 0 || pci > 0) {

			if(debug) {
				println("[Appositive] appositive detected")
				println("[Appositive] report = " + Describe.mentionInContext(rm, report))
				println("[Appositive] passage = " + Describe.mentionInContext(pm, passage))
			}

			// jaccard on word, lemma, ner
			// max over all partitions
			def appoMax(f: Token => String): Double = {
				val ll = fastJaccard(rtoks.take(rci).map(f), ptoks.take(pci).map(f), hsBuf)
				val lr = fastJaccard(rtoks.take(rci).map(f), ptoks.drop(pci).map(f), hsBuf)
				val rl = fastJaccard(rtoks.drop(rci).map(f), ptoks.take(pci).map(f), hsBuf)
				val rr = fastJaccard(rtoks.drop(rci).map(f), ptoks.drop(pci).map(f), hsBuf)
				math.max(math.max(ll, lr), math.max(rl, rr))
			}
			wordJaccard = appoMax(_.getWord)
			lemmaJaccard = appoMax(_.getLemma)
			nerJaccard = appoMax(_.getNerTag)

			// {1,2,3}-gram overlap
			def ngrams(n: Int, s: Seq[String]): Iterator[String] =
				s.sliding(n).map(_.mkString("+"))
			unigrams = fastIntersect(rtoks.map(_.getLemma).iterator, ptoks.map(_.getLemma).iterator, hsBuf)
			bigrams = fastIntersect[String](
				ngrams(2, rtoks.map(_.getLemma)),
				ngrams(2, ptoks.map(_.getLemma)), hsBuf)
			trigrams = fastIntersect[String](
				ngrams(3, rtoks.map(_.getLemma)),
				ngrams(3, ptoks.map(_.getLemma)), hsBuf)


			// head of each side of appositive
			try {
				val rParse = report.getSentence(rm).ptbParse.get
				val pParse = passage.getSentence(pm).ptbParse.get
				if(debug) {
					println("rci = " + rci)
					println("pci = " + pci)
					println("rtoks.idx = " + rtoks.map(_.index))
					println("ptoks.idx = " + ptoks.map(_.index))
				}
				lazy val ri = Seq(rtoks.head.index, rtoks(rci).index, rtoks.last.index+1)
				lazy val pi = Seq(ptoks.head.index, ptoks(pci).index, ptoks.last.index+1)
				lazy val rhl = largestConstituentIn(ri(0),   ri(1), rParse).headToken
				lazy val rhr = largestConstituentIn(ri(1)+1, ri(2), rParse).headToken
				lazy val phl = largestConstituentIn(pi(0),   pi(1), pParse).headToken
				lazy val phr = largestConstituentIn(pi(1)+1, pi(2), pParse).headToken

				if(rci > 0 && pci < 0) {
					headWordMatch |= rhl.getWord == phead.getWord
					headWordMatch |= rhr.getWord == phead.getWord
					headLemmaMatch |= rhl.getLemma == phead.getLemma
					headLemmaMatch |= rhr.getLemma == phead.getLemma
				}
				else if(rci < 0 && pci > 0) {
					headWordMatch |= rhead.getWord == phl.getWord
					headWordMatch |= rhead.getWord == phr.getWord
					headLemmaMatch |= rhead.getLemma == phl.getLemma
					headLemmaMatch |= rhead.getLemma == phr.getLemma
				}
				else {
					headWordMatch |= rhl.getWord == phl.getWord
					headWordMatch |= rhl.getWord == phl.getWord
					headWordMatch |= rhr.getWord == phr.getWord
					headWordMatch |= rhr.getWord == phr.getWord
					headLemmaMatch |= rhl.getLemma == phl.getLemma
					headLemmaMatch |= rhr.getLemma == phl.getLemma
					headLemmaMatch |= rhl.getLemma == phr.getLemma
					headLemmaMatch |= rhr.getLemma == phr.getLemma
				}
			}
			catch {
				case noParse: java.util.NoSuchElementException => {}
			}

			if(debug) {
				println("[Appsotive] word-jaccard = " + wordJaccard)
				println("[Appsotive] lemma-jaccard = " + lemmaJaccard)
				println("[Appsotive] ner-jaccard = " + nerJaccard)

				println("[Appsotive] unigrams = " + unigrams)
				println("[Appsotive] bigrams = " + bigrams)
				println("[Appsotive] trigrams = " + trigrams)

				println("[Appsotive] head-word = " + headWordMatch)
				println("[Appsotive] head-lemma = " + headLemmaMatch)
			}
		}


		b(sv, wordJaccard, "word-jaccard")
		b(sv, lemmaJaccard, "lemma-jaccard")
		b(sv, nerJaccard, "ner-jaccard")

		val k = 3d
		b(sv, unigrams/k, "unigrams")
		b(sv, bigrams/(k*k), "bigrams")
		b(sv, trigrams/(k*k*k), "trigrams")

		if(headWordMatch)
			b(sv, "head-word")
		if(headLemmaMatch)
			b(sv, "head-lemma")
	}

	/**
	 * chooses the biggest constituent that covers the given span
	 * for single token constituents, we break ties by choosing nouns first, verbs second, then other
	 */
	def largestConstituentIn(left: Int, right: Int, tree: Tree): Tree = {

		val debug = false
		if(debug) {
			println("[largestConstituentIn] left=%d right=%d".format(left, right))
			val l = tree.leftmostChild.headToken
			val r = tree.rightmostChild.headToken
			println("[largestConstituentIn] l.index=%d r.index=%d".format(l.index, r.index))
		}

		def score(constit: Tree): Double = {
			val t = constit.headToken
			val np = if(t.getPosTag.startsWith("N")) 2d else 0d	// nouns > verbs > other
			val vp = if(t.getPosTag.startsWith("V")) 1d else 0d
			2d*constit.width + np + vp + t.index/10d	// choose to the right
		}
		val pos = tree.leafNodes.filter(_.liesIn(left, right))
		var cur = pos.head
		var prev = cur
		while(cur.liesIn(left, right)) {
			prev = cur
			cur = cur.parent
		}
		if(prev.width == 1) pos.maxBy(score)
		else prev
	}

	private def fastJaccard[T](a: Traversable[T], b: Traversable[T], setBuf: java.util.Set[T]): Double = {
		var intersect = 0
		var union = 0
		setBuf.clear
		a.foreach(x => setBuf.add(x))
		b.foreach(x => {
			if(!setBuf.add(x))
				intersect = intersect + 1
			union = union + 1
		})
		if(union == 0) 0d
		else intersect.toDouble / union
	}

	private def fastIntersect[T](a: Iterator[T], b: Iterator[T], setBuf: java.util.Set[T]): Int = {
		var intersect = 0
		setBuf.clear
		a.foreach(x => setBuf.add(x))
		b.foreach(x => {
			if(!setBuf.add(x))
				intersect = intersect + 1
		})
		intersect
	}

}

