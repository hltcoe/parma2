// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.input

import edu.jhu.hlt.parma.annotation.PredArgSelector
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.util.AnnotationAligner.HalfAlignment
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.features.ReportingVerbs
import scala.collection.mutable.{ ArrayBuffer, HashSet, HashMap, Buffer }
import scala.collection.JavaConversions._
import java.io.File

object MTCDocAlignmentReader extends DocAlignmentReader[DocAlignment] with Logging2 {

	override def domain: String = "MTC"

	lazy val outOfDomainDocs = ConcreteDocAlignmentReader.EECB.getDocuments.toIndexedSeq

	override def getDocAlignments: Seq[DocAlignment] = {

		val all = getAllDocAlignments

		val lowOverlap = DocAlignmentPerturber.leastOverlapSubset(all, 0.5d)

		val das = lowOverlap.map(da =>
			DocAlignmentPerturber.degradeDocAlignment(da, outOfDomainDocs))

		val excuses = das.map(unsuitable(_))
		for((da, reasonsForExclusion) <- das.zip(excuses) if reasonsForExclusion.isEmpty)
			yield da
	}

	/** returns the reason it is unsuitable */
	def unsuitable(da: DocAlignment, tooManyPredArgs: Int = 80): Option[String] = {
		if((da.report.predicates.size + da.report.arguments.size) == 0 ||
			(da.passage.predicates.size + da.passage.arguments.size) == 0)
			return Some("no predicates or arguments in one of the documents")
		if(!(da.report.predicates.size < tooManyPredArgs && da.passage.predicates.size < tooManyPredArgs &&
			da.report.arguments.size < tooManyPredArgs && da.passage.arguments.size < tooManyPredArgs))
			return Some("too many predicates or arguments")
		if(da.sureAlignments.size == 0)
			return Some("no alignments")
		return None
	}


	/**
	 * read in all multiple translation document pairs (which already have GIZA alignments
	 * and other pre-processing done by Johnny)
	 */
	def getAllDocAlignments: Seq[ParametricDocAlignment[RichConcreteDocBuilder]] = {

		// read in documents from f1 and f2
		// go line by line in alignments.xml, make and accumulate alignments
		// every time you hit a new doc in alignments.xml, produce a DocAlignment

		log("[MTC getAllDocAlignments] reading in documents...")
		val f1f = ParmaConfig.getFile("data.mtc.reports")
		val f2f = ParmaConfig.getFile("data.mtc.passages")
		Profiler.startTask("mtc:docs")
		val docMap = Seq(f1f, f2f).flatMap(f => {
			log(f.getCanonicalPath)
			ConcreteWrapper.getCommunicationsFrom(f)
		}).map(d => (d.getGuid.getCommunicationId, d)).toMap
		val docTime = Profiler.endTask("mtc:docs")
		log("[MTC getAllDocAlignments] done, read in %d documents in %.1f seconds"
			.format(docMap.size, docTime / 1000d))

		val Doc = """<DOC id="(\S+)">""".r
		val Ignore = """(</?TEXT>|</DOC>)""".r
		val Alignment = """(\d+)-(\d+)""".r

		val alignments = new ArrayBuffer[ParametricDocAlignment[RichConcreteDocBuilder]]
		var alignmentSentence = 0	// running sentence index w.r.t. alignments

		var report: RichConcreteDocBuilder = null
		val reportHAs = new ArrayBuffer[HalfAlignment[Argument]]

		var passage: RichConcreteDocBuilder = null
		val passageHAs = new ArrayBuffer[HalfAlignment[Argument]]

		// HIGH LEVEL IDEA:
		// 1) read in doc id, look up documents
		// 2) call PredArgSelector, identify preds/args
		// 3) step through alignments, if matches existing pred/arg make HalfAlignment
		// 4) call AnnotationAligner.makeDocAlignment using HalfAlignments accumulated
		val af = ParmaConfig.getFile("data.mtc.token-alignments")
		var corefSet = 0
		val r = FileUtils.getReader(af)
		while (r.ready) {
			r.readLine.trim match {

				case Ignore(s) => {}

				// ==== NEW DOC PAIR ====
				case Doc(id) => { // ids in alignment file don't have .a and .b extensions

					assert((report == null) == (passage == null))
					if (report != null) {
						// dump previous alignment
						// 4) call AnnotationAligner.makeDocAlignment using HalfAlignments accumulated
						val a = AnnotationAligner.makeDocAlignment(
							report, reportHAs.toSeq,
							passage, passageHAs.toSeq,
							Some(domain), addPredArgs=false, strict=false)
						alignments += a
						log("[MTC getAllDocAlignments] #sure=%d #possible=%d"
							.format(a.sureAlignments.size, a.possibleAlignments.size))
						log("[MTC getAllDocAlignments] #sure-pred=%d #possible-pred=%d"
							.format(a.surePredicateAlignments.size, a.possiblePredicateAlignments.size))
						log("[MTC getAllDocAlignments] #sure-arg=%d #possible-arg=%d"
							.format(a.sureArgCorefAlignments.size, a.possibleArgCorefAlignments.size))
						log("")
					}

					// 1) read in doc id, look up documents
					// 2) call PredArgSelector, identify preds/args
					report = new RichConcreteDocBuilder(id + ".a", docMap(id + ".a"))
					report = PredArgSelector.identifyPredicatesAndArguments(report)
						.asInstanceOf[RichConcreteDocBuilder]
					reportHAs.clear

					passage = new RichConcreteDocBuilder(id + ".b", docMap(id + ".b").toBuilder.build)
					passage = PredArgSelector.identifyPredicatesAndArguments(passage)
						.asInstanceOf[RichConcreteDocBuilder]
					passageHAs.clear

					alignmentSentence = 0
					corefSet = 0
				}

				// ==== ALIGNMENT LINE ====
				case s => {
					assert(report != null && passage != null)

					// 3) step through alignments, if matches existing pred/arg make HalfAlignment
					Alignment.findAllMatchIn(s).foreach(m => {
						val rTokIdx = m.group(1).toInt
						val pTokIdx = m.group(2).toInt

						// we are assuming these are all sure alignments because
						// we have no scores to threshold on
						val isSure = true
						corefSet += 1
						
						// REPORT HalfAlignment
						//val rPredArg = report.predOrArgMatching(MentionBuilder.from(alignmentSentence, rTokIdx))
						val rPredArg = predOrArgMatching(MentionBuilder.from(alignmentSentence, rTokIdx), report)
						rPredArg match {
							case Some(epa) =>
								//log("[LeastOverlap] adding REPORT HA: " + epa)
								reportHAs += new HalfAlignment(epa, corefSet.toString, isSure)
							case None => {}	// no pred or arg matching this token
						}

						// PASSAGE HalfAlignment
						//val pPredArg = passage.predOrArgMatching(MentionBuilder.from(alignmentSentence, pTokIdx))
						val pPredArg = predOrArgMatching(MentionBuilder.from(alignmentSentence, pTokIdx), passage)
						pPredArg match {
							case Some(epa) =>
								//log("[LeastOverlap] adding PASSAGE HA: " + epa)
								passageHAs += new HalfAlignment(epa, corefSet.toString, isSure)
							case None => {}	// no pred or arg matching this token
						}
					})
					alignmentSentence += 1
				}
			}
		}
		r.close

		alignments.toSeq
	}

	def predOrArgMatching(m: Mention, d: Document): Option[Either[Predicate, Argument]] = {
		val p = d.predicates.filter(_.location == m).map(Left(_))
		val aa = d.corefs.flatMap(_.chain).filter(_.location == m).map(Right(_))
		val a = d.arguments.filter(_.location == m).map(Right(_))
		(a ++ aa ++ p).headOption
	}

}


