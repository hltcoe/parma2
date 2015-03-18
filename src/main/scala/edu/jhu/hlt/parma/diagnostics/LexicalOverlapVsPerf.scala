// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.diagnostics

import edu.jhu.hlt.parma.util.ParmaConfig
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder
import edu.jhu.hlt.parma.util.{FileUtils, Misc}
import edu.jhu.hlt.parma.evaluation.SetBasedEvaluator
import scala.collection.JavaConversions._
import java.io.File

class LexicalOverlapVsPerf(
		val name: String,
		val overlap: DocAlignment => Double,
		val performance: Instance[DocAlignment] => Double) {

	var writer: java.io.BufferedWriter = null

	def open {
		//val f = ParmaConfig.getFile("diagnostics.lexical-overlap-perf", null)
		//if(f != null) open(f)
		val dir = ParmaConfig.getDirectory("diagnostics.lexical-overlap-perf", null)
		if(dir != null && dir.exists)
			open(new File(dir, name+".txt"))
	}

	def open(f: File) {
		writer = FileUtils.getWriter(f)
	}

	def close {
		if(writer != null)
			writer.close
	}

	def analyze(instances: Traversable[Instance[DocAlignment]], desc: String) {
		if(writer != null) {
			instances.foreach(i => {
				val s = analyzeOne(i, desc)
				writer.write(s)
			})
			writer.flush
		}
	}
	
	def analyzeOne(inst: Instance[DocAlignment], desc: String): String = {
		val domain = inst.gold.domain match {
			case Some(d) => d
			case None => "unknown-domain"
		}
		val ov = overlap(inst.gold)
		val perf = performance(inst)
		"%.5f\t%.5f\t%s\t%s\n".format(ov, perf, domain, desc)
	}
}

object CosineVsF1 extends LexicalOverlapVsPerf("document-cosine-vs-f1",
	(da: DocAlignment) => {
		val f = (t: Token) => t.getWord.toLowerCase
		val r = Misc.toCounts(da.report.allTokens.map(f))
		val p = Misc.toCounts(da.passage.allTokens.map(f))
		Misc.cosine(r, p)
	},
	(inst: Instance[DocAlignment]) => SetBasedEvaluator.generousF1(inst)
)

object CosineBySentenceVsF1 extends LexicalOverlapVsPerf("sentence-cosine-vs-f1",
	(da: DocAlignment) => {
		val f = (t: Token) => t.getLemma
		val cosines = da.sureAlignments.map(a => {
			val (rCM, pCM) = CanonicalMentionFinder.canonicalMentions(a, da.context)
			val rs = da.report.getSentence(rCM)
			val ps = da.passage.getSentence(pCM)
			val r = Misc.toCounts(rs.tokens.map(f))
			val p = Misc.toCounts(ps.tokens.map(f))
			Misc.cosine(r, p)
		})
		cosines.sum / cosines.size.toDouble
	},
	(inst: Instance[DocAlignment]) => SetBasedEvaluator.generousF1(inst)
)




