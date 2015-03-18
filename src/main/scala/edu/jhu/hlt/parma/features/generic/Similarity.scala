// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features.generic

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util.{WikiRuleStore, WikiRule, Pair}
import edu.jhu.hlt.parma.features.TransducerSimilarityFeature
import edu.jhu.hlt.parma.features.wordnet.WordNet
import edu.jhu.hlt.parma.features.framenet.FrameNet
import edu.jhu.hlt.parma.features.ReportingVerbs
import no.priv.garshol.duke.comparators.JaroWinkler

class Similarity[D](name: String, val similarity: (Context, Pair[D]) => DVec, val outputDimension: Int)
	extends Pipe[(Context, Seq[Pair[D]]), Seq[DVec]](name, (t: (Context, Seq[Pair[D]])) => {
		val (c, spd) = t
		spd.map(pd => similarity(c, pd))
	}) {

	override def toString: String = "(Similarity %s dim=%d)".format(name, outputDimension)

	/**
	 * returns a similarity measure where every index in the returned
	 * DVec is multiplied by scale and then added to shift
	 */
	def linearTransform(scale: Double, shift: Double): Similarity[D] = {
		val newName = name + "$scaled"
		val newFunc = (c: Context, pd: Pair[D]) => similarity(c, pd).linearTransform(scale, shift)
		new Similarity(newName, newFunc, outputDimension)
	}

	def pmin = new Pipe[(Context, Seq[Pair[D]]), DVec](name + "pmin", (t: (Context, Seq[Pair[D]])) => {
		val sdv = this.apply(t)
		if(sdv.size < 1) {
			//throw new RuntimeException("min not defined over empty sets: " + name)
			// TODO switch to an agnostic value as in Binarizer
			DVec.zero(outputDimension)
		} else {
			val m = sdv.head.copy
			for(dv <- sdv.tail) m.minEquals(dv)
			m
		}
	})
	def pmax = new Pipe[(Context, Seq[Pair[D]]), DVec](name + "pmax", (t: (Context, Seq[Pair[D]])) => {
		val sdv = this.apply(t)
		if(sdv.size < 1) {
			//throw new RuntimeException("max not defined over empty sets: " + name)
			// TODO switch to an agnostic value as in Binarizer
			DVec.zero(outputDimension)
		} else {
			val m = sdv.head.copy
			for(dv <- sdv.tail) m.maxEquals(dv)
			m
		}
	})
	def pavg = new Pipe[(Context, Seq[Pair[D]]), DVec](name + "pavg", (t: (Context, Seq[Pair[D]])) => {
		val sdv = this.apply(t)
		if(sdv.size < 1) {
			//throw new RuntimeException("avg not defined over empty sets: " + name)
			// TODO switch to an agnostic value as in Binarizer
			DVec.zero(outputDimension)
		} else {
			val m = sdv.head.copy
			for(dv <- sdv.tail) m += dv
			m *= (1d / sdv.size)
			m
		}
	})
}

object SimilarityImplementation {
	
	implicit def double2dvec(v: Double) = new DVec(v)
	implicit def bool2dvec(b: Boolean) = new DVec(if(b) 1d else 0d)
	implicit def bool2double(b: Boolean) = if(b) 1d else 0d

	def setup {
		_wordnet.setup
		_framenet = FrameNet.getInstance
		//WikiRuleStore.setup
		_reportingVerbs.setup(java.util.Collections.emptyList[DocAlignment])
	}

	private def featuresForRule[T](wr: WikiRule[T]) = {
		new DVec(wr.lhsCount, wr.rhsCount, wr.jointCount)
		throw new RuntimeException("implement me")
	}
	
	val wikiRules = new Similarity[String]("wikirules", (c: Context, p: Pair[String]) => {
		val lIntRules = WikiRuleStore.lhsMatches(p.left)
		val rIntRules = WikiRuleStore.lhsMatches(p.right)
		val lRules = WikiRuleStore.mergeRules(lIntRules)
		val rRules = WikiRuleStore.mergeRules(rIntRules)

		// for(rules <- List(int, string))
		//     left in rules(right)
		//     right in rules(left)
		//     intersect rules(left) and rules(right)

		// TODO
		// don't forget about link types and counts!

		val a = lRules.filter(_.rhs == p.right)
		val b = rRules.filter(_.rhs == p.left)
		val asymmetricStr = new DVec(a.size + b.size > 0,
									a.size + b.size > 1,
									a.size > 1 && b.size > 1)
		
		val intersectStrs = lRules.map(_.rhs).toSet & rRules.map(_.rhs).toSet
		val symmetricStr = new DVec(intersectStrs.size > 0, intersectStrs.size > 1, intersectStrs.size > 2)
		
		asymmetricStr ++ symmetricStr
	}, 6)
	
	private val _wordnet = new WordNet
	val wordNet = new Similarity[String]("wordnet", (c: Context, p: Pair[String]) => {
		val maxD = 2
		// TODO shortest paths caching
		// TODO test that
		val a = _wordnet.getHolonymDistance(p.left, p.right, maxD)
		val b = _wordnet.getHypernymDistance(p.left, p.right, maxD)
		val c = _wordnet.getHyponymDistance(p.left, p.right, maxD)
		val d = _wordnet.getMeronymDistance(p.left, p.right, maxD)
		val e = _wordnet.getSynonymDistance(p.left, p.right, maxD)
		new DVec(a==1, a==2,
				b==1, b==2,
				c==1, c==2,
				d==1, d==2,
				e==1, e==2)
	}, 10)//.withCaching	// NOTE: this really doens't help! i've benchmarked it twice, not enough cache hits

	def wordNetAndStringSim(name: String, sim: (String, String) => Double, threshold: Double) =
			new Similarity[String]("wordNetAnd" + name, (c: Context, p: Pair[String]) => {
		val maxD = 2
		val a = _wordnet.getHolonymDistance(p.left, p.right, maxD)
		val b = _wordnet.getHypernymDistance(p.left, p.right, maxD)
		val c = _wordnet.getHyponymDistance(p.left, p.right, maxD)
		val d = _wordnet.getMeronymDistance(p.left, p.right, maxD)
		val e = _wordnet.getSynonymDistance(p.left, p.right, maxD)
		val w = sim(p.left, p.right)
		new DVec(Array(a==1 || w > threshold,
							b==1 || w > threshold,
							c==1 || w > threshold,
							d==1 || w > threshold,
							e==1 || w > threshold).map(b => if(b) 1d else 0d))
	}, 5)//.withCaching	// NOTE: this really doens't help! i've benchmarked it twice, not enough cache hits

	def wordNetAndNameTransducer(thresh: Double) = wordNetAndStringSim("Transducer-"+thresh, _nameTransducer.score, thresh)
	val wordNetAndStringEq = wordNetAndStringSim("StringEq", (a:String,b:String) => if(a equalsIgnoreCase b) 1d else 0d, 0.5d)
	def wordNetAndJaroWinkler(thresh: Double) = wordNetAndStringSim("JaroWinkler-"+thresh, (a:String,b:String) => JaroWinkler.similarity(a,b), thresh)
	
	private var _framenet: FrameNet = null
	val frameNet = new Similarity[String]("framenet", (c: Context, p: Pair[String]) => {
		val maxD = 3
		val a = _framenet.getChildDistance(p.left, p.right, maxD)
		val b = _framenet.getParentDistance(p.left, p.right, maxD)
		val c = _framenet.getPerspectiveChildDistance(p.left, p.right, maxD)
		val d = _framenet.getPerspectiveParentDistance(p.left, p.right, maxD)
		new DVec(a==1, a==2,
				b==1, b==2,
				c==1, c==2,
				d==1, d==2)
	}, 8)//.withCaching	// NOTE: this really doens't help! i've benchmarked it twice, not enough cache hits
	
	private var _reportingVerbs = new ReportingVerbs
	val reportingVerbs = new Similarity[String]("reportingVerbs", (c: Context, p: Pair[String]) => {
		val l = _reportingVerbs.isReportingVerb(p.left)
		val r = _reportingVerbs.isReportingVerb(p.right)
		new DVec(l^r, l&&r)
	}, 2)
	
	val stringEq = new Similarity[String]("stringEq", (c: Context, ps: Pair[String]) => ps.left equalsIgnoreCase ps.right, 1)
	
	val firstLetterMatch = new Similarity[String]("firstLetterMatch", (c: Context, ps: Pair[String]) => ps.left(0) == ps.right(1), 1)
	
	type Sequence = Seq[String]
	val firstLetterMatchSeq = new Similarity[Sequence]("firstLetterSeq", (c: Context, p: Pair[Sequence]) => {
		if(p.left.size > 0 && p.left.size == p.right.size) {
			val x = p.left.zip(p.right)
			x.forall(xy => xy._1 == xy._2)
		}
		else DVec.zero1
	}, 1)
	
	val longestCommonSubsequence = new Similarity[Sequence]("lcs", (c: Context, p: Pair[Sequence]) => {
		throw new RuntimeException("implement me")
	}, 1)
	
	val levenshtein = new Similarity[Seq[String]]("levenshtein", (c: Context, p: Pair[Seq[String]]) => {
		def f(s: String, sb: StringBuilder, hm: java.util.HashMap[String, java.lang.Integer]) {
			val old = hm.get(s)
			if(old == null) {
				val nw = hm.size
				hm.put(s, nw)
				sb.append(nw.toChar)
			}
			else sb.append(old.toChar)
		}
		val hm = new java.util.HashMap[String, java.lang.Integer]
		val s1 = new StringBuilder
		val s2 = new StringBuilder
		p.left.foreach(f(_, s1, hm))
		p.right.foreach(f(_, s2, hm))
		new DVec(levenshteinDistance(s1.toString, s2.toString))
	}, 1)

	def levenshteinDistance(s1: String, s2: String): Double = {
		val ca1 = s1.toCharArray
		val ca2 = s2.toCharArray
		val Ni = ca1.length
		val Nj = ca2.length
		if(Ni == 0) return Nj
		if(Nj == 0) return Ni
		val table = Array.ofDim[Int](Ni, Nj)
		var i = 0
		while(i < Ni) {
			var j = 0
			while(j < Nj) {
				if     (i == 0 && j == 0) table(i)(j) = 0
				else if(i == 0 && j != 0) table(i)(j) = j
				else if(i != 0 && j == 0) table(i)(j) = i
				else {
					val a = table(i-1)(j-1) + (if(ca1(i) == ca2(j)) 0 else 1)
					val b = table(i-1)(j)   + 1
					val c = table(i)(j-1)   + 1
					if(a <= b && a <= c)
						table(i)(j) = a
					else if(b <= a && b <= c)
						table(i)(j) = b
					else
						table(i)(j) = c
				}
				j = j + 1
			}
			i = i + 1
		}
		table(Ni-1)(Nj-1)
	}
	
	// multi-set to multi-set similairty
	val intersect = new Similarity[Set[String]]("intersect", (c: Context, p: Pair[Set[String]]) => {
		(p.left & p.right).size
	}, 1)
	
	def bothContain(word: String) = new Similarity[Set[String]]("contains-"+word, (c: Context, p: Pair[Set[String]]) => {
		val one = p.left.contains(word) ^ p.right.contains(word)
		val both = p.left.contains(word) && p.right.contains(word)
		new DVec(one, both)
	}, 2)
	
	val jaccard = new Similarity[Set[String]]("jaccard", (c: Context, p: Pair[Set[String]]) => {
		val intersect = p.left & p.right
		val union = p.left | p.right
		if(union.size == 0) 0d
		else intersect.size.toDouble / union.size.toDouble
	}, 1)
	
	val jaroWinklerDistance = new Similarity[String]("jaroWinklerDistance", (c: Context, ps: Pair[String]) => {
		JaroWinkler.similarity(ps.left, ps.right)
	}, 1)
	
	private val _nameTransducer = new TransducerSimilarityFeature
	val nameTransducer = new Similarity[String]("nameTransducer", (c: Context, ps: Pair[String]) => {
		_nameTransducer.score(ps.left, ps.right)
	}, 1)
}

