// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util

import edu.jhu.hlt.parma.evaluation._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.inference._
import edu.jhu.hlt.parma.inference.maxmargin.Constraint
import edu.jhu.hlt.parma.util.AnnotationAligner.HalfAlignment
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder
import collection.JavaConversions._
import collection.mutable.Buffer
import java.util.Date

/**
 * functions that generate strings useful for inspecting data 
 */
object Describe extends Logging2 {

	def constraint(c: Constraint): String = {
		val df = c.getDeltaFeatures
		"(Constraint loss=%.3f df.l1=%g df.l2=%g df.lInf=%g)"
			.format(c.getLoss, df.l1, df.l2, df.lInf)
	}

	def svec(sv: SVec): String = {
		val sb = new StringBuilder
		sb.append("(sv")
		for((i, v) <- sv.items)
			sb.append(" %d:%.1f".format(i, v))
		sb.append(")")
		sb.toString
	}

	def features(sv: SVec, engine: InferenceEngine[_], compact: Boolean = true): String = {
		val sb = new StringBuilder
		val vec = if(compact) {
				val v = SVec.duplicate(sv)
				v.compact
				v
			}
			else sv
		for((i, v) <- vec.items) {
			val name = engine.featureName(i)
			sb.append("%d\t%.120s\t%.2f\n".format(i, name, v))
		}
		sb.toString
	}

	def linearDecision(features: SVec, parameters: DVec, featureNames: Int => String,
			numRelevant: Int = 15): String = {
		val sb = new StringBuilder
		val dp = VecOps.dot(parameters, features)
		// find the the most relevant features-parameter pairs
		sealed class Index(val idx: Int, val featureWeight: Double, val featureValue: Double) {
			def effectMagnitude: Double = math.abs(effect)
			def effect: Double = featureWeight * featureValue
		}
		val eps = 1e-5
		val pairs = features.items.map(iv =>
				new Index(iv._1, parameters(iv._1), iv._2)
			).filter(_.effectMagnitude > 1e-5)
			.toBuffer[Index]
			.sortBy(- _.effectMagnitude)
		pairs.takeRight(numRelevant).foreach(index => {
			val name = featureNames(index.idx)
			sb.append("%+.3f\t= w=%+.3f * f=%+.3f \t %s\n"
				.format(index.effect, index.featureWeight, index.featureValue, name))
		})
		sb.append("... %d more features ...\n".format(pairs.size - numRelevant))
		//sb.append("final decision: sign(dot=%.3f) = (%s)\n".format(dp, if(dp > 0d) "+" else "-"))
		sb.toString
	}

	def document(doc: Document): String = {
		val sb = new StringBuilder
		sb.append("Document id=%s\n".format(doc.id))
		for((p,i) <- doc.predicates.zipWithIndex)
			sb.append("\tpredicate(%d) %s\n".format(i+1, predicate(p, doc)))
		for((a,i) <- doc.arguments.zipWithIndex)
			sb.append("\targument(%d) %s\n".format(i+1, argument(a, doc)))
		sb.toString
	}
		
	def docAlignmentInstance(dai: Instance[DocAlignment]): String = {
		val sb = new StringBuilder
		sb.append("eval all:        " +
			SetBasedEvaluator.generousF1(dai, take=SetBasedEvaluator.All) + "\n")
		sb.append("eval predicates: " +
			SetBasedEvaluator.generousF1(dai, take=SetBasedEvaluator.Preds) + "\n")
		sb.append("eval arguments:  " +
			SetBasedEvaluator.generousF1(dai, take=SetBasedEvaluator.Args) + "\n")
		sb.append("<<<<<< Hyp  >>>>>>\n")
		sb.append(docAlignment(dai.hyp))
		sb.append("\n")
		sb.append("<<<<<< Gold >>>>>>\n")
		sb.append(docAlignment(dai.gold))
		sb.append("\n")
		sb.toString
	}
	
	def docAlignment(da: DocAlignment): String =  {
		val buf = new StringBuilder
		buf ++= "Report: " + da.report.id + "\t"
		buf ++= "Passage: " + da.passage.id + "\n"
		buf ++= "Predicates:\n"
		val sortedPredAlignments = da.possiblePredicateAlignments.toBuffer
			.sortBy((pa: PredicateAlignment) => alignment(pa, da.report, da.passage))
		for(pa <- sortedPredAlignments) {
			val reportString = mentionInContext(pa.reportPred.location, da.report)
			val passageString = mentionInContext(pa.passagePred.location, da.passage)
			buf ++= "\t\"%s\" <=> \"%s\"\n".format(reportString, passageString)
		}
	   
		buf ++= "Argument Coref Alignments:\n"
		val sortedArgCorefAlignments = da.possibleArgCorefAlignments.toBuffer
			.sortBy((aca: ArgCorefAlignment) => alignment(aca, da.report, da.passage))
		for(arg <- sortedArgCorefAlignments) {
			val rCanonicalMention = CanonicalMentionFinder.canonicalMention(arg.reportCoref, da.report)
			val pCanonicalMention = CanonicalMentionFinder.canonicalMention(arg.passageCoref, da.passage)
			for(rArgument <- arg.reportCoref; pArgument <- arg.passageCoref) {
		
				//val reportString = da.report.getMentionString(rArgument)
				val reportString = mentionInContext(rArgument.location, da.report)
				//val passageString = da.passage.getMentionString(pArgument)
				val passageString = mentionInContext(pArgument.location, da.passage)
			
				val rCanonical = if(rCanonicalMention == rArgument) "Canonical " else ""
				val pCanonical = if(pCanonicalMention == pArgument) "Canonical " else ""

				buf ++= "\t\"%s\" %s <=> \"%s\" %s\n"
					.format(reportString, rCanonical, passageString, pCanonical)
			}
		}

		for((aa, aaIdx) <- da.possibleArgCorefAlignments.zipWithIndex) {
			if(aa.reportCoref.size + aa.passageCoref.size > 2) {
				val (reportCM, passageCM) = CanonicalMentionFinder.canonicalMentions(aa, da.report, da.passage)
				buf ++= "================= ArgCorefAlignment %d ====================\n".format(aaIdx)
				buf ++= "\t report caonical mention  = %s\n".format(mentionInContext(reportCM, da.report))
				buf ++= "\t passage caonical mention = %s\n".format(mentionInContext(passageCM, da.passage))
				buf ++= "\t ========= report chain =========\n"
				for(m <- aa.reportCoref)
					buf ++= "\t\t %s\n".format(mentionInContext(m.location, da.report))
				buf ++= "\t ========= passage chain =========\n"
				for(m <- aa.passageCoref)
					buf ++= "\t\t %s\n".format(mentionInContext(m.location, da.passage))
			}
		}
		buf ++= "\n"
		buf.toString
	}
	

	def argCoref(argc: ArgumentCoref, doc: Document, contextWordsEachSide: Int = 3): String = {
		val cm = CanonicalMentionFinder.canonicalMention(argc, doc)
		val headStr = mentionInContext(cm.location, doc, contextWordsEachSide)
		val restStrs = argc.filter(_ != cm).map(a => mentionInContext(a.location, doc, contextWordsEachSide))
		if(restStrs.size > 0)
			"(ArgCoref in %s: head=\"%s\" rest=%s)".format(doc.id, headStr, restStrs.mkString(", "))
		else "(ArgCoref in %s: \"%s\")".format(doc.id, headStr)
	}
	
	
	def argument(arg: Argument, doc: Document, contextWordsEachSide: Int = 3): String = {
		"(Argument in %s: %s)".format(doc.id, mentionInContext(arg.location, doc, contextWordsEachSide))
	}

	
	def predicate(pred: Predicate, doc: Document, contextWordsEachSide: Int = 3): String = {
		"(Predicate in %s: %s)".format(doc.id, mentionInContext(pred.location, doc, contextWordsEachSide))
	}
	
	
	//def alignment(a: Alignment, c: Context, contextWordsEachSide: Int = 3): String = alignment(a, c.report, c.passage, contextWordsEachSide=contextWordsEachSide)
	def alignment(a: Alignment, report: Document, passage: Document, contextWordsEachSide: Int = 3): String = a match  {
		case aca: ArgCorefAlignment => "(Alignment\n\treport=%s\n\tpassage=%s\n)".format(
				argCoref(aca.reportCoref, report, contextWordsEachSide),
				argCoref(aca.passageCoref, passage, contextWordsEachSide))
		case pa: PredicateAlignment => "(Alignment\n\treport=%s\n\tpassage=%s\n)".format(
				predicate(pa.reportPred, report, contextWordsEachSide),
				predicate(pa.passagePred, passage, contextWordsEachSide))
		case _ => throw new RuntimeException
	}


	def justHeadWord(a: Alignment, report: Document, passage: Document): String = {
		val (reportCM, passageCM) = CanonicalMentionFinder.canonicalMentions(a, report, passage)
		val f = (d: Document, m: Mention) => "%s(%s)".format(d.getHeadString(m), d.getHeadToken(m).getLemma)
		"%s\t%s".format(f(report, reportCM), f(passage, passageCM))
	}
	
	private def mentionWithHeadHighlighted(m: Mention, sent: Sentence): String = {
		var foundHead = false
		//val s = doc.getMentionTokens(m).zipWithIndex.map(ti => {
		val s = sent.tokens.slice(m.getStartTokenIdx, m.getEndTokenIdx).zipWithIndex.map(ti => {
			val (tok, idx) = ti
			if(m.getStartTokenIdx + idx == m.getHeadTokenIdx) {
				foundHead = true
				"*" + tok.getWord + "*"
			}
			else tok.getWord
		}).mkString(" ")
		if(!foundHead) {
			warn("start=%d end=%d head=%d sentence=%s"
				.format(m.getStartTokenIdx, m.getEndTokenIdx, m.getHeadTokenIdx,
					Describe.sentence(sent)))
			warn("couldn't find head!")
		}
		assert(foundHead)
		s
	}
	
	def mentionInContext(m: Mention, doc: Document, contextWordsEachSide: Int = 3): String =
		mentionInContext(m, doc.getSentence(m), contextWordsEachSide)
	def mentionInContext(m: Mention, sent: Sentence, contextWordsEachSide: Int): String = {
		if(sent.index != m.getSentenceIdx) throw new RuntimeException("wrong sentence")
		val sb = new StringBuilder
		val left = sent.before(m)
		if(left.size > contextWordsEachSide)
			sb.append("... ")
		sb.append(left.takeRight(contextWordsEachSide).map(_.getWord).mkString(" "))
		sb.append(" [" + mentionWithHeadHighlighted(m, sent) + "] ")
		val right = sent.after(m)
		sb.append(right.take(contextWordsEachSide).map(_.getWord).mkString(" "))
		if(right.size > contextWordsEachSide)
			sb.append(" ...")
		sb.toString
	}
	
	def halfAlignment(ha: HalfAlignment[Argument], doc: Document): String = {
		val ms = ha.mention match {
			case Left(p) =>
				Describe.predicate(p, doc)
			case Right(a) =>
				Describe.argument(a, doc)
		}
		"(HalfAlignment corefId=%s isSure=%s %s)".format(ha.corefSet, ha.isSure, ms)
	}
	
	def sentence(sent: Sentence): String = {
		val p1 = sent.tokens.map(t => "%s_{%s,%s}".format(t.getWord, t.getPosTag, t.getNerTag)).mkString(" ")
		val p2 = sent.dependencies.map(d => d.map(t => t.getWord).toString).mkString("\n\t")
		"(Sentence \"%s\"\n\t dependencies = (%s))\n".format(p1, p2)
	}

	def memoryUsage(timestamp: Boolean = false): String = {
		val r = Runtime.getRuntime
		def toGB(by: Long): Double = by/1024d/1024d/1024d
		if(timestamp) {
			"(mem used:%.1fG free:%.1fG limit:%.1fG at %s)"
				.format(toGB(r.totalMemory), toGB(r.freeMemory), toGB(r.maxMemory), new Date().toString)
		}
		else {
			"(mem used:%.1fG free:%.1fG limit:%.1fG)"
				.format(toGB(r.totalMemory), toGB(r.freeMemory), toGB(r.maxMemory))
		}
	}
	
}
