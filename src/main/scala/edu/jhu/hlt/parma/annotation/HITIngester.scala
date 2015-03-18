// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.annotation

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.input._
import edu.jhu.hlt.parma.inference._
import edu.jhu.hlt.parma.util.AlignmentFileUtil.AlignmentRef
import edu.jhu.hlt.parma.util.MentionFileUtil.MentionRef
import edu.jhu.hlt.concrete.Concrete.Communication
import java.io._
import collection.mutable.{ ArrayBuffer, HashSet }

/**
 * given a mention file, alignment file, and Communications (without preds/arg),
 * first read in alignments as DocAlignments, then save them in a
 * parma format which stores Discoures and Communications (with preds/args)
 *
 * TODO handle non-singletons (alignments right now are over singletons)
 */
object HITIngester extends Logging2 {

	// description that goes into DocAlignments, providence information
	var domain = "TurkScores"

	val verbose = true

	def main(args: Array[String]) {
		if(args.length < 4 || args.length > 5) {
			println("please provide:")
			println("1) a .alignment file")
			println("2) a .mentions file")
			println("3) a concrete communications file (e.g. from the anno pipeline + concrete-agiga)")
			println("4) a file to dump (Discourse,Communication,Communication)+ tuples to")
			println("5) [optional] a domain string to assign to these DocAlignments")
			return
		}

		val alignmentFile = new File(args(0))
		val mentionsFile = new File(args(1))
		val communicationsFile = new File(args(2))
		val daFile = new File(args(3))
		if(args.length == 5)
			domain = args(4)

		val das = readAlignments(communicationsFile, mentionsFile, alignmentFile)
		ConcreteDocAlignmentUtils.serialize(das, daFile)
	}

	/**
	 * we get at least 2 HIT ids that correspond to a given doc pair
	 * preds and args are handled in different HITs
	 */
	class MultiHITAlignment(val allAlignments: Seq[JoinedAlignment]) {
		val byHit: Map[String, Seq[JoinedAlignment]] = allAlignments.groupBy(_.hit)
		def predHits: Seq[String] = {
			for((hit, alignments) <- byHit.toSeq
				if alignments.filter(_.isPredicate).size > 0)
					yield hit
		}
		def argHits: Seq[String] = {
			for((hit, alignments) <- byHit.toSeq
				if alignments.filter(_.isArgument).size > 0)
					yield hit
		}
		/**
		 * for now, just prefer alignments that are heavily filled in
		 */
		def scoreHit(hitId: String): Double = byHit(hitId).size.toDouble
		def alignments: Seq[AlignmentRef] = {
			val p = predHits
			val a = argHits
			assert(a.size > 0, "there are no hits to cover arguments")
			assert(p.size > 0, "there are no hits to cover predicate")
			assert((p.toSet & a.toSet.toSet).size == 0, "hits should be *either* preds or args, right?")
			byHit(p.maxBy(scoreHit)) ++ byHit(a.maxBy(scoreHit)) map(_.alignment)
		}
		def reportDocId: String = {
			val rids = allAlignments.map(_.reportMention.docId).toSet
			assert(rids.size == 1)
			rids.head
		}
		def passageDocId: String = {
			val pids = allAlignments.map(_.passageMention.docId).toSet
			assert(pids.size == 1)
			pids.head
		}
	}

	/**
	 * you can't know if an alignment is a pred or arg alignment unless you get the mentions
	 */
	case class JoinedAlignment(val alignment: AlignmentRef, val reportMention: MentionRef, val passageMention: MentionRef) {
		require(reportMention.kind == passageMention.kind)
		def isPredicate: Boolean = reportMention.isPredicate
		def isArgument: Boolean = !isPredicate
		def hit: String = alignment.hitId
	}

	/**
	 * choose an appropriate set of alignments for every pair of documents
	 */
	def teeUpAlignments(mentions: Map[String, MentionRef], alignmentFile: File): Seq[MultiHITAlignment] = {
		val alignments = AlignmentFileUtil.readAlignmentsFrom(alignmentFile)
		val joinedAlignments = alignments.map(a => JoinedAlignment(a, mentions(a.reportMentionId), mentions(a.passageMentionId)))
		def docIdPair(ja: JoinedAlignment): (String, String) = (ja.reportMention.docId, ja.passageMention.docId)
		for(((rDocId, pDocId), alignments) <- joinedAlignments.groupBy(docIdPair).toSeq)
			yield new MultiHITAlignment(alignments)
	}

	def readAlignments(communicationsFile: File, mentionsFile: File, alignmentFile: File): Seq[DocAlignment] = {
		val docs: Map[String, Communication] = ConcreteWrapper.getCommunicationsFrom(communicationsFile).map(d => (d.getGuid.getCommunicationId, d)).toMap
		val mentions: Map[String, MentionRef] = MentionFileUtil.readMentionsFrom(mentionsFile).map(mr => (mr.id, mr)).toMap
		val das = new ArrayBuffer[DocAlignment]
		for((docPair: MultiHITAlignment) <- teeUpAlignments(mentions, alignmentFile)) {
			val reportComm = docs(docPair.reportDocId)
			val passageComm = docs(docPair.passageDocId)
			das += makeDocAlignment(docPair.alignments, reportComm, passageComm, mentions)
		}
		das.toSeq
	}

	/**
	 * @deprecated
	 * naive, doesn't consider multiple HITs that are judgements of the same doc alignment pair
	 * will have 0 predicate alignments or 0 arg alignments (hopefully not both though...)
	 */
	def readAlignmentsOld(communicationsFile: File, mentionsFile: File, alignmentFile: File): Seq[DocAlignment] = {
		val docs = ConcreteWrapper.getCommunicationsFrom(communicationsFile).map(d => (d.getGuid.getCommunicationId, d)).toMap

		val alignments = AlignmentFileUtil.readAlignmentsFrom(alignmentFile, header=false)
		val mentions = MentionFileUtil.readMentionsFrom(mentionsFile, header=false).map(mr => (mr.id, mr)).toMap

		// group alignments by assignmentId,
		// foreach make a DocAlignment (requires looking up mentions => preds/args)
		// score the DocAlignment with HAM
		val das = new ArrayBuffer[DocAlignment]
		for((assignmentId, tAlignments) <- alignments.groupBy(_.hitId)) {
			val ta = tAlignments.head
			try {
				val report = docs(mentions(ta.reportMentionId).docId)
				val passage = docs(mentions(ta.passageMentionId).docId)

				// within an assignment, we should be talking about one document pair
				warnIf(report.getGuid.getCommunicationId == passage.getGuid.getCommunicationId,
					"report.guid == passage.guid: \"%s\"".format(report.getGuid.getCommunicationId))
				assert(tAlignments.map(ta => docs(mentions(ta.reportMentionId).docId).getGuid.getCommunicationId).toSet.size == 1)
				assert(tAlignments.map(ta => docs(mentions(ta.passageMentionId).docId).getGuid.getCommunicationId).toSet.size == 1)

				das += makeDocAlignment(tAlignments, report, passage, mentions)
			}
			catch {
				case nse: java.util.NoSuchElementException =>
					println("WARNING: could not look up a mention id in: " + ta)
			}
		}
		das.toSeq
	}

	private def makeDocAlignment(tAlignments: Seq[AlignmentRef],
			reportAD: Communication, passageAD: Communication,
			mentions: Map[String, MentionRef]): DocAlignment = {

		// make a new builder each time so that preds/args from another
		// assignment/task are not added and stored permanently
		// put another way: the preds/args that appear in one assignment/task
		// may not always be the same
		val report = new RichConcreteDocBuilder(reportAD)
		val passage = new RichConcreteDocBuilder(passageAD)

		logIf(verbose, "report=%s passage=%s #alignments=%d #possible=%d #sure=%d"
			.format(report.id, passage.id, tAlignments.size, tAlignments.filter(_.isPossible).size, tAlignments.filter(_.isSure).size))

		// need to add args/preds, but exactly once (alignments may reference a given pred/arg more than once)
		val reportArgs = new HashSet[ArgumentCoref]
		val reportPreds = new HashSet[Predicate]
		val passageArgs = new HashSet[ArgumentCoref]
		val passagePreds = new HashSet[Predicate]

		// scan turk-alignments for 1) real alignments and 2) the mentions that were visible
		val sureAlignments = new ArrayBuffer[Alignment]
		val possibleAlignments = new ArrayBuffer[Alignment]
		for(ta <- tAlignments) {
			val m1 = mentions(ta.reportMentionId)
			val m2 = mentions(ta.passageMentionId)

			assert(report.id == m1.docId)
			assert(passage.id == m2.docId)
			assert(m1.kind == m2.kind)

			if(m1.isPredicate) {
				val rp = new Predicate(m1.toMention)
				val pp = new Predicate(m2.toMention)
				reportPreds += rp
				passagePreds += pp
				if(!ta.isNotAligned) {
					(if(ta.isSure) sureAlignments
					 else possibleAlignments) += PredicateAlignment(rp, pp)
				}
			} else {
				val ra = new ArgumentCoref(Argument(m1.toMention))
				val pa = new ArgumentCoref(Argument(m2.toMention))
				reportArgs += ra
				passageArgs += pa
				if(!ta.isNotAligned) {
					(if(ta.isSure) sureAlignments
					 else possibleAlignments) += ArgCorefAlignment(ra, pa)
				}
			}
		}

		// add preds/args to the documents
		// for now we're assuming turkers alignments are all singletons
		reportPreds.foreach(report.addPredicate)
		passagePreds.foreach(passage.addPredicate)
		reportArgs.foreach(report.addCoref)
		passageArgs.foreach(passage.addCoref)

		val id = "r%s_p%s".format(report.id, passage.id)
		val da = new ParametricDocAlignment(id, Some(domain), report, passage, sureAlignments.toSet, possibleAlignments.toSet)
		println("[TurkerScorer makeDocAlignment] " + Describe.docAlignment(da))
		da
	}

}

