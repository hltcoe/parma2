// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util

import java.io.File
import scala.collection.mutable.{Buffer, ArrayBuffer}
import scala.collection.JavaConversions._

class WikiRule[T](val lhs: T, val rhs: T,
			val ruleTypes: Seq[String],
			val ruleRank: Double,
			val paths: Seq[String],
			val lhsCount: Int,
			val rhsCount: Int,
			val jointCount: Int) {

	override def toString: String = "(WikiRule %s->%s [%s])".format(lhs, rhs, ruleTypes.mkString(","))

	override def equals(o: Any): Boolean = {
		if(o.isInstanceOf[WikiRule[_]]) {
			val owr = o.asInstanceOf[WikiRule[_]]
			lhsCount == owr.lhsCount &&
				rhsCount == owr.rhsCount &&
				jointCount == owr.jointCount &&
				ruleRank == owr.ruleRank &&
				lhs == owr.lhs &&
				rhs == owr.rhs &&
				ruleTypes == owr.ruleTypes &&
				paths == owr.paths
		}
		false
	}

	override def hashCode: Int = {
		var hc = 0
		hc ^= (lhsCount << 24)
		hc ^= (rhsCount << 16)
		hc ^= (jointCount << 8)
		hc ^= (lhs.hashCode << 4)
		hc ^= rhs.hashCode
		hc
	}
	
	/**
	 * to calculate PMI you need the total number of documents,
	 * which is not available in this data
	 * since this is based on wikipedia, and my scrape from 2009
	 * had 23M articles, that is what i'm assuming here
	 */
	def pmi(corpusSize: Double = 23e6) = {
//		val pxy = jointCount / corpusSize
//		val px = lhsCount / corpusSize
//		val py = rhsCount / corpusSize
//		math.log(pxy) - math.log(py) - math.log(px)
		math.log(jointCount * ((corpusSize / lhsCount) / rhsCount))
	}
	
	def dice = 2d*jointCount / (lhsCount + rhsCount)
	def condOnLHS = jointCount / lhsCount.toDouble
	def condOnRHS = jointCount / rhsCount.toDouble
}

object WikiRuleStore {
	import java.util.HashMap
	val lhsIndex = new HashMap[Int, Buffer[WikiRule[Int]]]
	val rhsIndex = new HashMap[Int, Buffer[WikiRule[Int]]]
	val term2ids = new HashMap[String, Buffer[Int]]
	val id2term = new ArrayBuffer[String]

	def setup {
		val terms = ParmaConfig.getFile("features.wikirules.terms")
		val rules = ParmaConfig.getFile("features.wikirules.rules")
		val ruleCounts = ParmaConfig.getFile("features.wikirules.ruleCounts")
		setup(terms, rules, ruleCounts)
	}

	def setup(termsFile: File, rulesFile: File, ruleCountsFile: File) {

		Profiler.startTask("wikiRulesIO")

		// read in terms
		println("[WikiRuleStore] reading terms...")
		val tr = FileUtils.getReader(termsFile)
		while(tr.ready) {
			val ar = tr.readLine.trim.split(",", 2)
			if(ar.length == 2) {
				val idx = Integer.parseInt(ar(0))
				if(idx >= id2term.size) {
					(id2term.size to idx).foreach(i => id2term += null)
					assert(idx < id2term.size)
				}
				id2term(idx) = ar(1)

				var i = term2ids.get(ar(1))
				val u = (i==null)
				if(u) i = new ArrayBuffer[Int]
				i += idx
				if(u) term2ids.put(ar(1), i)
			}
			//else println("skipping " + ar.mkString(","))
		}
		tr.close

		// read in rules
		println("[WikiRuleStore] reading rules...")
		var added = 0
		val tempIdx = new HashMap[(Int, Int), WikiRule[Int]]
		val rr = FileUtils.getReader(rulesFile)
		while(rr.ready) {
			val ar = rr.readLine.trim.split(",")
			if(ar.length != 5 && ar.length != 4)
				println("ar = " + ar.mkString(", "))
			val lhs = Integer.parseInt(ar(0))
			val rhs = Integer.parseInt(ar(1))
			val ruleTypes = ar(2).split("@")
								.map(x => Map("1" -> "Redirect",
			                                  "2" -> "BeComplement",
			                                  "3" -> "Parenthesis",
			                                  "4" -> "Link",
			                                  "5" -> "AllNouns")(x))
			val ruleRank = ar(3).toDouble
			val paths = if(ar.length == 5) ar(4).split(":") else new Array[String](0)

			added += 1
			val rule = new WikiRule(lhs, rhs, ruleTypes, ruleRank, paths, -1, -1, -1)	// set counts later
			val before = tempIdx.put((lhs,rhs), rule)
			if(before != null)
				println("lhs=%s, rhs=%s, before=%s, after=%s, added=%d, line=[%s]".format(lhs, rhs, before, rule, added, ar.mkString(", ")))
		}
		rr.close
		println("after reading rules, tempIdx.size=%d, added=%d".format(tempIdx.size, added))

		// read in the counts
		println("[WikiRuleStore] adding counts...")
		added = 0
		val cr = FileUtils.getReader(ruleCountsFile)
		while(cr.ready) {
			var ar = cr.readLine.trim.split(",")
			val lhs = Integer.parseInt(ar(0))
			val rhs = Integer.parseInt(ar(1))
			val lhsCount = Integer.parseInt(ar(2))
			val rhsCount = Integer.parseInt(ar(3))
			val jointCount = Integer.parseInt(ar(4))
			// last one is dice, not needed
			
			// update counts, move from tempIdx to lhs/rhsIndex
			val oldRule = tempIdx.get((lhs, rhs))
			if(oldRule != null) {
				val rule = new WikiRule(oldRule.lhs, oldRule.rhs, oldRule.ruleTypes, oldRule.ruleRank, oldRule.paths, lhsCount, rhsCount, jointCount)
				//println("lhs=%s, rhs=%s, rule=%s".format(lhs, rhs, rule))

				var l = lhsIndex.get(lhs)
				var u = (l==null)
				if(u) l = new ArrayBuffer[WikiRule[Int]]
				l += rule
				if(u) lhsIndex.put(lhs, l)

				var r = rhsIndex.get(rhs)
				u = (r==null)
				if(u) r = new ArrayBuffer[WikiRule[Int]]
				r += rule
				if(u) rhsIndex.put(rhs, r)

				added += 1
			}
		}
		cr.close
		val time = Profiler.endTask("wikiRulesIO")
		println("read in %d rules and found %d with counts in %.1f seconds".format(tempIdx.size, added, time/1000d))
	}

	def lookup(idx: Int): Option[String] = {
		val t = id2term(idx)
		if(t == null) None
		else Some(t)
	}

	def lhsMatches(s: String): Seq[WikiRule[Int]] = {
		val ids = term2ids.get(s)
		if(ids == null) Seq()
		else ids.flatMap(id => lhsIndex.get(id))
	}
	
	def rhsMatches(s: String): Seq[WikiRule[Int]] = {
		val ids = term2ids.get(s)
		if(ids == null) Seq()
		else ids.flatMap(id => rhsIndex.get(id))
	}

	/**
	 * if there are two rules:
	 * lhs=(42,   "foo")
	 * lhs=(8734, "foo")
	 * with associated RHS rules and types,
	 * combine them to one rule with string LHS/RHS
	 * merge counts too
	 *
	 * scrubWikiTitles makes lhs="George Washington (president)" go to "George Washington"
	 */
	def mergeRules(rules: Seq[WikiRule[Int]], lhs: Boolean = true, rhs: Boolean = false, scrubWikiTitles: Boolean = true): Seq[WikiRule[String]] = {
		var uniq = 0
		val r2k = (rule: WikiRule[Int]) => {
			val lhsStr = lookup(rule.lhs).get
			val rhsStr = lookup(rule.rhs).get
			val l = if(scrubWikiTitles) scrubWikiTitle(lhsStr) else lhsStr
			val r = if(scrubWikiTitles) scrubWikiTitle(rhsStr) else rhsStr
			if(lhs && rhs)
				(l, r)
			else if(lhs && !rhs) {
				uniq += 1
				(l, "UNIQ-"+uniq)
			}
			else if(!lhs && rhs) {
				uniq += 1
				("UNIQ-"+uniq, r)
			}
			else throw new RuntimeException
		}
		val collapsed = new ArrayBuffer[WikiRule[String]]
		for((k, lr) <- rules.groupBy(r2k)) {
			val ruleRank = lr.map(_.ruleRank).sum / lr.size	// TODO think about alternatives
			val ruleTypes = lr.flatMap(_.ruleTypes).toSet.toSeq
			val paths = lr.flatMap(_.paths).toSet.toSeq
			val lhsCounts = lr.map(_.lhsCount).sum
			val rhsCounts = lr.map(_.rhsCount).sum
			val jointCounts = lr.map(_.jointCount).sum
			collapsed ++= lr.map(r => {
				val lh = lookup(r.lhs).get
				val rh = lookup(r.rhs).get
				new WikiRule[String](lh, rh, ruleTypes, ruleRank, paths, lhsCounts, rhsCounts, jointCounts)
			})
		}
		collapsed
	}

	def scrubWikiTitle(s: String): String = {
		val WikiTitle = """^(.+)\s+\(.+?\)$""".r
		s.trim match {
			case WikiTitle(t) => t
			case _ => s
		}
	}
}

