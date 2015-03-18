// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.diagnostics.GeneralDiagnostics
import no.priv.garshol.duke.comparators.JaroWinkler
import scala.math
import scala.collection.mutable.{ Buffer, ArrayBuffer, HashSet, HashMap }
import scala.collection.JavaConversions._
import java.io._

/**
 * Tool for taking pre-existing annotations, such as from the TAC Entity Linking
 * datasets, and aligning them against an document (as best as possible).
 */
object AnnotationAligner extends Logging2 {

	class HalfAlignment[ArgType](val mention: Either[Predicate, ArgType], val corefSet: String, val isSure: Boolean) {
		override def toString = "(HalfAlignment mention=%s, corefSet=%s, isSure=%s)".format(mention, corefSet, isSure)
	}

	val verbose = false
	
	/**
	 * merges Arguments by corefSet into ArgumentCorefs
	 * passes through predicate HalfAlignments
	 *
	 * assumes that if you have a possible alignment, then you use
	 * a distinct corefSet to represent those alignments
	 *
	 * TODO remote addPredArgs and always add predArgs (need to
	 * update classes that use this with addPredArgs=false)
	 */
	private def mergeHalfAlignments(mentionList: Seq[HalfAlignment[Argument]], doc: DocumentBuilder,
			addPredArgs: Boolean, strict: Boolean = true): scala.collection.Map[String, HalfAlignment[ArgumentCoref]] = {

		val debug = false
		if(debug) {
			println("[mergeHalfAlignments] top of fxn:")
			for(ha <- mentionList) println(ha)
			println()
		}

		// add predicates and arguments
		// note that some annotations will have two different corefSets
		// for the same pred/arg, and right now DocumentBuilder complains
		// about this. Alignments are now handled here (DocumentBuilder
		// needs to be updated to not care about/store corefSets), so just
		// make sure that we only add a given mention once.
		// bug/feature: this means that a given mention cannot be both a
		// predicate and and argument (seems fair).
		if(addPredArgs) {
			if(debug) println("[AnnotationAligner] merging HAs for " + doc.id)
			val pasAdded = new java.util.HashSet[Mention]
			for(ha <- mentionList) {
				ha.mention match {
					case Left(pa) =>
						if(pasAdded.add(pa.location)) {
							if(debug) println("[AnnotationAligner] adding: " + pa.location)
							doc.addPredicate(pa)
						}
						else if(debug) println("[AnnotationAligner] not adding location due to collision: " + pa.location)
					//case Right(aa) =>
					//	if(pasAdded.add(aa.location))
					//		doc.addArgument(new Argument(aa.location))
					case Right(aa) => {}	// add later as ArgumentCoref
				}
			}
		}

		// partition into predicate and argument halfalignments
		val (p, a) = mentionList.partition(_.mention match {
			case Left(p) => true
			case Right(a) => false
		})
		
		// make a map based on corefset (for preds and args)
		val pm = p.groupBy(_.corefSet).mapValues(s => {
			// predicates are singletons
			// TODO EECB has some instances of predicate coref
			// in their annotations, add this capability
			if(s.size > 1 && strict) {
				println("[mergeHalfAlignments] WARNING: saw instance of predicate coref,"
					+ " which we currently don't handle. Add this capability!")
				for((key, value) <- p.groupBy(_.corefSet))
					if(value.size > 1)
						println("%s => %s".format(key, value))
				assert(false)
			}
			s.head
		}).asInstanceOf[Map[String, HalfAlignment[ArgumentCoref]]]
		
		// merge args into argcorefs
		val am = a.groupBy(_.corefSet).mapValues(ha => {
			var sure = true
			val corefs = new HashSet[String]
			val argCoref = new ArgumentCoref
			ha.foreach(h => {
				sure &= h.isSure
				corefs.add(h.corefSet)
				h.mention match {
					case Left(p) => throw new RuntimeException
					case Right(a) => argCoref.chain += a
				}
			})
			assert(corefs.size == 1)
			if(addPredArgs)
				doc.addCoref(argCoref)
			new HalfAlignment(Right(argCoref), corefs.head, sure)
		}).asInstanceOf[Map[String, HalfAlignment[ArgumentCoref]]]
		
		// merge maps into common coref-keyed map
		val r = new HashMap[String, HalfAlignment[ArgumentCoref]]
		for((c, a) <- am)
			r += (c -> a)
		for((c, p) <- pm) {
			// in a given document, should not have two predicates or
			// argCorefs aligned to the same corefset. multiple args
			// is possible, but by now they should be rolled up into an argCoref
			assert(!r.keySet.contains(c))
			r += (c -> p)
		}
		r
	}
	
	/**
	 * does not add Predicates or Arguments to the provided Documents
	 */
	def makeDocAlignment[D <: DocumentBuilder](
			report: D, reportMentionList: Seq[HalfAlignment[Argument]],
			passage: D, passageMentionList: Seq[HalfAlignment[Argument]],
			domain: Option[String] = None, addPredArgs: Boolean, strict: Boolean = true): ParametricDocAlignment[D] = {

		if(verbose) {
			println("[makeDocAlignment] report = " + report.id)
			println("[makeDocAlignment] passage = " + passage.id)
		}

		// merge HalfAlignment[Argument]s into HalfAlignment[ArgCoref]s
		val reportMentions = mergeHalfAlignments(reportMentionList, report, addPredArgs, strict)
		val passageMentions = mergeHalfAlignments(passageMentionList, passage, addPredArgs, strict)
		
		val sure = new HashSet[Alignment]
		val possible = new HashSet[Alignment]
		for(corefSet <- reportMentions.keySet & passageMentions.keySet) {
			val rHA = reportMentions(corefSet)
			val pHA = passageMentions(corefSet)

			val a = (rHA.mention, pHA.mention) match {
				case (Left(rp), Left(pp)) =>
					new PredicateAlignment(rp, pp)
				case (Right(ra), Right(pa)) =>
					new ArgCorefAlignment(ra, pa)
				case (Left(rp), Right(pa)) =>	// translate to predicate?
					if(strict) {
						println("[makeDocAlignments] WARNING: rp =" + Describe.predicate(rp, report))
						println("[makeDocAlignments] WARNING: pa =" + Describe.argCoref(pa, passage))
						assert(false)
					}
					null
				case (Right(ra), Left(pp)) =>	// translate to predicate?
					if(strict) {
						println("[makeDocAlignments] WARNING: ra =" + Describe.argCoref(ra, report))
						println("[makeDocAlignments] WARNING: pp =" + Describe.predicate(pp, passage))
						assert(false)
					}
					null
				case _ =>
					warn("encountered a predicate aligned to an argument (corefSet=%s, report=%s, passage=%s), skipping"
						.format(rHA.corefSet, report.id, passage.id))
					null
			}
			if(a != null) {
				if (rHA.isSure && pHA.isSure) sure.add(a)
				else possible.add(a)
			}
		}
		new ParametricDocAlignment[D]("r%s_p%s".format(report.id, passage.id), domain, report, passage, sure.toSet, possible.toSet)
	}

	/**
	 * @deprecated -- you shouldn't be using something like this unless you're
	 *   writing a non-Concrete DocReader, and you shouldn't be doing that :)
	 *   also, there is a better method impelmented in EECBDocReader
	 *
	 * Given a sentence and a target mention's String tokenization, such as created
	 * based on the EECB annotation, find the mention that already exists in the
	 * document that most closely matches it.
	 *
	 * implementation:
	 * iterate over all constituents of this sentence and choose
	 * the one with the smallest Jaro-Winkler distance to the
	 * input string
	 */
	def alignMentionString(sentence: Sentence, targetString: String): Mention = {
		sentence.ptbParse match {
			case Some(tree) =>
				tree.nodes.map(n => {
					(n, JaroWinkler.similarity(n.textSpan, targetString))
				}).maxBy(_._2)._1.mentionSpan
			case None =>
				throw new RuntimeException("this needs a parse to work")
		}
	}

}

