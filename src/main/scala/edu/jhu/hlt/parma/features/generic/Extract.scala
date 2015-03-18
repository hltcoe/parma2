// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features.generic

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util.Pair
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder
import scala.collection.JavaConversions._

class Extract[T](name: String, val extract: ((Context, Alignment)) => (Context, Seq[Pair[T]]))
	extends Pipe[(Context, Alignment), (Context, Seq[Pair[T]])](name, extract) {
	
	def this(name: String, t: (Context, Alignment) => (Context, Seq[Pair[T]])) =
		this(name, (tp: (Context, Alignment)) => t(tp._1, tp._2))
	
	def compose[R](p: Pipe[T,R]): Extract[R] = compose(name+"-"+p.name, p.func)
	def compose[R](newName: String, f: T => R): Extract[R] = {
		new Extract[R](newName, (tp: (Context, Alignment)) => {
			val (context, spt) = extract(tp)
			(context, spt.map(_.map(f)))
		})
	}
}

object ExtractorImplementation {
	
	val canonicalMention = new Extract[Mention]("canonicalMention", (context: Context, a: Alignment) => {
		val (rcm, pcm) = CanonicalMentionFinder.canonicalMentions(a, context.report, context.passage)
		(context, Seq(new Pair(rcm, pcm)))
	})
	
	val allMentions = new Extract[Mention]("allMentions", (context: Context, a: Alignment) => {
		val pairs: Seq[Pair[Mention]] = a match {
			case pa: PredicateAlignment =>
				// features will have already looked at canonicalMention, this is redundant
				//Seq(new Pair(pa.reportPred, pa.passagePred).map(_.location))
				Seq[Pair[Mention]](new Pair(pa.reportPred.location, pa.passagePred.location))
			case aca: ArgCorefAlignment =>
				aca.reportCoref.flatMap(r => aca.passageCoref.map(p => new Pair(r, p).map(_.location))).toSeq
		}
		(context, pairs)
	})
	
	/**
	 * report = "A savvy [band] of jewel thieves ..."
	 * passage = "Armed [robbers] ..."
 	 *   => since "thieves" is governed by "band", we want to check
	 *   "robbers" against "thieves"
	 */
	val asymmetricHeadGovDep = new Extract[Set[Token]]("asymmetricHeadGovDep", (c: Context, a: Alignment) => {
		val (rcm, pcm) = CanonicalMentionFinder.canonicalMentions(a, c)
		val rt = c.report.getHeadToken(rcm)
		val pt = c.passage.getHeadToken(pcm)
		(c, Seq(new Pair(Set(pt), c.report.governs(rcm).map(_.gov).toSet),
			new Pair(Set(pt), c.report.governedBy(rcm).map(_.dep).toSet),
			new Pair(Set(rt), c.passage.governs(pcm).map(_.gov).toSet),
			new Pair(Set(rt), c.passage.governs(pcm).map(_.dep).toSet)))
	})
	
	val sentenceTokensAfterCM = new Extract[Set[Token]]("sentenceTokensAfterCM", (context: Context, a: Alignment) => {
		val (rcm, pcm) = CanonicalMentionFinder.canonicalMentions(a, context)
		val rs = context.report.getSentence(rcm).after(rcm)
		val ps = context.passage.getSentence(pcm).after(pcm)
		(context, Seq(new Pair(rs.toSet, ps.toSet)))
	})
	
	val sentenceTokensBeforeCM = new Extract[Set[Token]]("sentenceTokensBeforeCM", (context: Context, a: Alignment) => {
		val (rcm, pcm) = CanonicalMentionFinder.canonicalMentions(a, context)
		val rs = context.report.getSentence(rcm).before(rcm)
		val ps = context.passage.getSentence(pcm).before(pcm)
		(context, Seq(new Pair(rs.toSet, ps.toSet)))
	})
	
	val sentenceTokens = new Extract[Set[Token]]("sentenceTokens", (context: Context, a: Alignment) => {
		val (rcm, pcm) = CanonicalMentionFinder.canonicalMentions(a, context)
		val rs = context.report.getSentence(rcm).tokens
		val ps = context.passage.getSentence(pcm).tokens
		(context, Seq(new Pair(rs.toSet, ps.toSet)))
	})
	
	// for canonical mention
	val rootToCM = new Extract[Seq[Tree]]("dominationPath", (c: Context, a: Alignment) => {
		val (rcm, pcm) = CanonicalMentionFinder.canonicalMentions(a, c)
		val rs = c.report.getSentence(rcm)
		val ps = c.passage.getSentence(pcm)
		(rs.ptbParse, ps.ptbParse) match {
			case (Some(rp), Some(pp)) =>
				val rht = c.report.getHeadToken(rcm)
				val rdp = rp.dominationPath(rht)
				
				val pht = c.passage.getHeadToken(pcm)
				val pdp = pp.dominationPath(pht)
				
				(c, Seq(new Pair(rdp, pdp)))
			case _ =>
				println("warning: could not find PTB parses in " + c)
				(c, Seq())
		}
	})
	
	// TODO SRL, nom-bank, etc
	// TODO commonParent(a,b) = deepest node c s.t. dominates(c, a) and dominates(c, b)
}

