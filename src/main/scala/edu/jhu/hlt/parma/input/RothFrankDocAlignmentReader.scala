// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.input

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.util.AnnotationAligner.HalfAlignment
import edu.jhu.hlt.parma.diagnostics.GeneralDiagnostics
import edu.jhu.hlt.concrete.Concrete.Communication
import edu.jhu.hlt.concrete.io.ProtocolBufferReader
import scala.io._
import scala.collection.JavaConversions._
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, Buffer}
import java.io._
import java.util.zip.GZIPInputStream

/**
 * the purpose of this code is to remove duplicate mentions
 * for a given pred/arg because when we serialize to Concrete
 * we must have a uniq mapping between text spans (Mentions or TokenRefSequence)
 * and Predicates/SituationMentions or Arguments/EntityMention
 */
case class M1(val location: Mention, val isSure: Boolean, val corefSet: Int, val isPred: Boolean)
case class M2(val location: Mention, val isPred: Boolean)	// uniq in the doc
case class Link[M](val rm: M, val pm: M, val isSure: Boolean) {
	def map[T](rf: M => T, pf: M => T): Link[T] = Link[T](rf(rm), pf(pm), isSure)
}
object NewWay {
	def buildAlignment(rMentions: Seq[M1], pMentions: Seq[M1],
			reportComm: Communication, passageComm: Communication): DocAlignment = {

		// get all the positive alignments/links how they need to be
		val rCollapse: Map[M1, M2] = collapse(rMentions)
		val pCollapse: Map[M1, M2] = collapse(pMentions)
		val links: Seq[Link[M1]] = extract(rMentions, pMentions)

		// construct the doc alignment
		val report = new RichConcreteDocBuilder(reportComm)
		val passage = new RichConcreteDocBuilder(passageComm)
		for((m2: M2) <- rCollapse.values.toSet)
			if(m2.isPred) report.addPredicate(new Predicate(m2.location))
			else report.addArgument(new Argument(m2.location))
		for((m2: M2) <- pCollapse.values.toSet)
			if(m2.isPred) passage.addPredicate(new Predicate(m2.location))
			else passage.addArgument(new Argument(m2.location))

		val daId = "r%s_p%s".format(report.id, passage.id)
		val domain = Some("RF")
		val collapseLinks: Seq[Link[M2]] = links.map(_.map(rCollapse.apply, pCollapse.apply))
		val sure = new ArrayBuffer[Alignment]
		val possible = new ArrayBuffer[Alignment]
		implicit def m2toMention(m2: M2) = m2.location
		for(link2 <- collapseLinks) {
			val a = if(link2.rm.isPred || link2.rm.isPred)
				new PredicateAlignment(new Predicate(link2.rm), new Predicate(link2.pm))
			else {
				new ArgCorefAlignment(
					new ArgumentCoref(new Argument(link2.rm)),
					new ArgumentCoref(new Argument(link2.pm)))
			}
			if(link2.isSure) sure += a
			else possible += a
		}
		new DocAlignment(daId, domain, report, passage, sure.toSet, possible.toSet)
	}
	/**
	 * take a bunch of mentions that may not be uniq by location
	 * and project them down to a smaller set that is
	 *
	 * when we collapse, we loose in-doc coref information.
	 * i'm assuming that there is none because if you're using
	 * 2 different coref labels to indicate a sure and possible link
	 * leaving a pred/arg, that you are not going to have one or both
	 * of those coref sets match up to other stuff in the same document.
	 * this is absurd on its face because an item would surely need to
	 * be coreferent with itself (and this sure/possible coref label
	 * scheme makes this impossible)!
	 */
	def collapse(mentions: Seq[M1]): Map[M1, M2] = {
		val byLoc = new collection.mutable.HashMap[Mention, M2]
		mentions.map(m =>
			(m, byLoc.getOrElseUpdate(m.location, M2(m.location, m.isPred)))
		).toMap
	}
	/**
	 * look at the corefSet infomration in the mentions and generate
	 * links between M1s
	 */
	def extract(rMentions: Seq[M1], pMentions: Seq[M1]): Seq[Link[M1]] = {
		// group by coref set
		val rm = rMentions.groupBy(_.corefSet)
		val pm = pMentions.groupBy(_.corefSet)
		val buf = new ArrayBuffer[Link[M1]]
		for(k <- rm.keys.toSet & pm.keys.toSet) {
			for(r <- rm(k); p <- pm(k)) {
				assert(r.isSure == p.isSure)
				buf += Link[M1](r, p, r.isSure)
			}
		}
		buf.toSeq
	}
}

class RothFrankDocAlignmentReader(var takeDev: Boolean, var takeTest: Boolean)
		extends DocAlignmentReader[DocAlignment]
		//extends DocAlignmentReader[ParametricDocAlignment[RichConcreteDocBuilder]]
		with Logging2 {
	
	def this() = this(true, false)
	
	val pedantic = false

	override def domain = "RF"
	
	override def getDocAlignments: Seq[DocAlignment] = getDocAlignmentsAlt

	def getDocAlignmentsOld: Seq[ParametricDocAlignment[RichConcreteDocBuilder]] = {
		val docStore = ConcreteWrapper.getCommunicationsFrom(ParmaConfig.getFile("data.rf.concrete.documents"))
			.map(c => (c.getGuid.getCommunicationId -> c)).toMap
		val annotationFile = ParmaConfig.getFile("data.rf.annotations")
		val alignments = new ArrayBuffer[ParametricDocAlignment[RichConcreteDocBuilder]]
		val source = Source.fromFile(annotationFile)
		val iter = source.getLines
		
		// oh imperative programming!
		var pairDir = iter.next
		var report, passage: RichConcreteDocBuilder = null
		var reportMentions, passageMentions: Buffer[HalfAlignment[Argument]] = null
		var oldPairDir: String = null
		var reportName: String = null
		var passageName: String = null

		if(pedantic && !(takeDev ^ takeTest))
			warn("you probably don't want to take both train *and* test alignments from this data!")
		
		val f2n = (f: String) => f.replace("XML/", "").replace("_", "").replace("/", ".")
		
		while(pairDir != null) {
			
			reportName = iter.next
			report = new RichConcreteDocBuilder(f2n(pairDir+"1"), docStore(reportName))
			
			reportMentions = new ArrayBuffer[HalfAlignment[Argument]]
			passageName  = readAlignments(iter, report, reportMentions)		// add alignments from 1.xml
			
			passage = new RichConcreteDocBuilder(f2n(pairDir+"2"), docStore(passageName))
			
			oldPairDir = pairDir
			passageMentions = new ArrayBuffer[HalfAlignment[Argument]]
			pairDir = readAlignments(iter, passage, passageMentions)			// add alignments from 2.xml
			
			
			// TODO
			// there is some dependency in what you set the document name
			// to and the mentions that get added. if i run the code as-is
			// (i.e. with new Document(f2n(pairDir+"1"), docStore(aDoc1Name)))
			// then it works. if i put it back to how i had it for chris
			// and MTurkUtils, it breaks. fix this.
			
			
			val da = AnnotationAligner.makeDocAlignment(report, reportMentions, passage, passageMentions, Some(domain), addPredArgs=true)
			if(takeDev && oldPairDir.contains("dev"))
				alignments += da
			if(takeTest && oldPairDir.contains("test"))
				alignments += da
		}
		source.close
		
		log(GeneralDiagnostics.docAlignmentStatistics(alignments, "debug, takeDev=%s takeTest=%s".format(takeDev, takeTest)))
		alignments.toSeq
	}
	
	
	/**
	 * calls addAPredArg for every line that is an alignment
	 * and returns the last line that isn't an alignment
	 * (or null if iterator is empty)
	 */
	private def readAlignments(iter: Iterator[String], doc: RichConcreteDocBuilder, storeMentionsIn: Buffer[HalfAlignment[Argument]]): String = {
		var ret: String = null
		while(ret == null) {
			if(!iter.hasNext)
				return null
			val line = iter.next
			if(!addAPredArg(line, doc, storeMentionsIn))
				ret = line
		}
		ret
	}


	/**
	 * tries to find a word given only a paragraph and sentence idx
	 * returns (sentenceIdx, tokenIdx)
	 */
	private def findWord(word: String, doc: Document, paragraph: Int, occurrence: Int): (Int, Int) = {
		var seen = 0
		var r = (-1, -1)
		for((sent, sentenceIdx) <- doc.sentences.zipWithIndex.drop(paragraph)) {	// there is at least one sentence per paragraph
			for((tok, tokIdx) <- sent.tokens.zipWithIndex) {
				val w = tok.getWord
				if(w.contains(word)) {
					seen += 1
					if(occurrence+1 == seen) {
						if(r._1 >= 0 && r._2 >= 0)
							throw new RuntimeException("DATA IS AMBIGUOUS!")
						r = (sentenceIdx, tokIdx)
					}
				}
			}
		}
		if(r == (-1,-1)) {
			warn("doc id = " + doc.id)
			warn("doc = " + Describe.document(doc))
			warn("word = " + word)
			warn("paragraph = " + paragraph)
			warn("occurrence = " + occurrence)
			assert(false)
		}
		assert(r._1 >= 0 && r._2 >= 0)
		assert(r._1 < doc.sentences.size)
		assert(r._2 < doc.sentences(r._1).size)
		r
	}
	




	// <<<< START UPDATED METHOD >>>> (handles args)
	private sealed class RFMention(val docPairId: String, val docId: String,
			val paragraph: Int, val occurrence: Int, val word: String, val corefSet: Int, val confidence: String) {
		require(docPairId != null)
		require(docId != null)
		require(Set("possible", "sure", "none").contains(confidence))
		var isPredicate: Option[Boolean] = None
		def isSure: Boolean = "sure" == confidence
		override def toString: String = "(RFMention pair=%s docId=%s para=%d occ=%d word=%s corefSet=%d conf=%s)"
			.format(docPairId, docId, paragraph, occurrence, word, corefSet, confidence)
	}
	private def getMentions(f: File): Seq[RFMention] = {
		val mentions = new ArrayBuffer[RFMention]
		var pairId: String = null
		var docId: String = null
		val MPat = """(\d+) (\d+) (\S+) (\d+) (possible|sure|none)""".r
		val PPat = """XML/(\w+)/(\w+)/""".r
		val DPat = """(AFP|APW)_ENG_(.*)""".r
		val s = io.Source.fromFile(f)
		s.getLines.foreach(_ match {
			case PPat(ds, pair) =>
				pairId = ds + "/" + pair
			case DPat(ds, date) =>
				docId = ds + "_ENG_" + date
			case MPat(para_s, occ_s, word, corefSet, conf) =>
				val (para, occ) = (para_s.toInt - 1, occ_s.toInt - 1)
				assert(occ >= 0)
				// this data will provide some preds/args that have 's at the end
				// which is usually split in annotated gigaword: strip it off
				// e.g. "country's" vs ("country" "'s")
				val wordToUse = 
					if(word.endsWith("'s"))
						word.substring(0, word.size - 2)
					else word
				mentions += new RFMention(pairId, docId, para.toInt, occ.toInt, wordToUse, corefSet.toInt, conf)
		})
		s.close
		println("mentions = " + mentions.mkString("\n"))
		mentions.toSeq
	}
	private def findMentionInDoc(rfm: RFMention, doc: Document): Mention = {
		// the Roth and Frank data indexes words by their paragraph
		// but annotated gigaword only provides sentence indices
		// this is a moderately-safe approximate matching method
		val (sentenceIdx, tokIdx) = findWord(rfm.word, doc, rfm.paragraph, rfm.occurrence)
		val (left, right, head) = (tokIdx, tokIdx+1, tokIdx)
		MentionBuilder.from(sentenceIdx, left, right, head)
	}
	private def makeHalfMention(rfm: RFMention, doc: Document): Option[HalfAlignment[Argument]] = {
		require(!rfm.isPredicate.isEmpty)
		if(rfm.confidence == "none") None
		else {
			val m = findMentionInDoc(rfm, doc)	// TODO caching
			val pOrA: Either[Predicate, Argument] =
				if(rfm.isPredicate.get) Left(new Predicate(m))
				else Right(new Argument(m))
			Some(new HalfAlignment(pOrA, rfm.corefSet.toString, rfm.confidence == "sure"))
		}
	}
	def getDocAlignmentsAlt: Seq[DocAlignment] = {
		// RFMention => Mention => HalfAlignment => AnnotationAligner
		val rfMentions = getMentions(ParmaConfig.getFile("data.rf.annotations"))
		val docMap: Map[String, Communication] =
			ConcreteWrapper.getCommunicationsFrom(ParmaConfig.getFile("data.rf.concrete.documents"))
			.map(c => (c.getGuid.getCommunicationId -> c)).toMap

		// group by (doc-pair, corefSet)
		// if any of the elements are a V* or JJ*, then all are predicates
		for(((pairId, corefSet), mentions) <- rfMentions.groupBy(rfm => (rfm.docPairId, rfm.corefSet))) {
			val isPredCluster = !mentions.forall(rfm => {
				val doc = new RichConcreteDocBuilder(docMap(rfm.docId))
				//log("docId=%s, sentences=%s".format(doc.id, doc.sentences.mkString("\n")))
				val m = findMentionInDoc(rfm, doc)
				val t = doc.getHeadToken(m)
				warnIf(t.getWord != rfm.word, "token=%s annotation=%s".format(t.getWord, rfm.word))
				val pos = t.getPosTag
				!(pos.startsWith("V") || pos.startsWith("JJ"))
			})
			mentions.foreach(_.isPredicate = Some(isPredCluster))
		}

		// group first by pair id, then doc id
		val newWay = true
		val alignments = new ArrayBuffer[DocAlignment]
		for((pairId, mentions) <- rfMentions.groupBy(_.docPairId)) {
			mentions.groupBy(_.docId).toList match {
				case (reportId, reportMentions) :: (passageId, passageMentions) :: Nil =>

					val da = if(newWay) {
						// need 2 Communications and 2 sets of M1s
						// RFMention --findMentionInDoc--> Mention --m2m1--> M1
						// at this point, we assume all args are singletons? => YES
						val report: Communication = docMap(reportId)
						val passage: Communication = docMap(passageId)
						val reportDoc = new RichConcreteDocBuilder(report)
						val passageDoc = new RichConcreteDocBuilder(passage)
						val reportM1s = reportMentions.map(rfm => {
							val location: Mention = findMentionInDoc(rfm, reportDoc)
							M1(location, rfm.isSure, rfm.corefSet, rfm.isPredicate.get)
						})
						val passageM1s = passageMentions.map(rfm => {
							val location: Mention = findMentionInDoc(rfm, passageDoc)
							M1(location, rfm.isSure, rfm.corefSet, rfm.isPredicate.get)
						})
						NewWay.buildAlignment(reportM1s, passageM1s, report, passage)
					}
					else {
						val report = new RichConcreteDocBuilder(docMap(reportId))
						val passage = new RichConcreteDocBuilder(docMap(passageId))
						val reportHAs = reportMentions.flatMap(rfm => makeHalfMention(rfm, report))
						val passageHAs = passageMentions.flatMap(rfm => makeHalfMention(rfm, passage))
						AnnotationAligner.makeDocAlignment(report, reportHAs, passage, passageHAs, Some(domain), addPredArgs=true)
					}
					if(takeDev && pairId.contains("dev"))
						alignments += da
					if(takeTest && pairId.contains("test"))
						alignments += da

				case x =>
					throw new RuntimeException("are there more than two docs in a pair? " + x)
			}
		}
		log(GeneralDiagnostics.docAlignmentStatistics(alignments, "debug, takeDev=%s takeTest=%s".format(takeDev, takeTest)))
		alignments.toSeq
	}
	// <<<< END UPDATED METHOD >>>>








	
	/**
	 * returns true if the line matches the format of an alignment line
	 */
	private def addAPredArg(line: String, doc: RichConcreteDocBuilder,
			storeMentionsIn: Buffer[HalfAlignment[Argument]]): Boolean = {
		
		val Pat = """(\d+) (\d+) (\S+) (\d+) (possible|sure|none)""".r
		line match {
			case Pat(para_s, occurrence_s, word, corefSet, conf) => {
				val (para, occurrence) = (para_s.toInt - 1, occurrence_s.toInt - 1)
				assert(occurrence >= 0)

				// this data will provide some preds/args that have 's at the end
				// which is usually split in annotated gigaword: strip it off
				// e.g. "country's" vs ("country" "'s")
				val wordToUse = 
					if(word.endsWith("'s"))
						word.substring(0, word.size - 2)
					else word

				// the Roth and Frank data indexes words by their paragraph
				// but annotated gigaword only provides sentence indices
				// this is a moderately-safe approximate matching method
				val (sentenceIdx, tokIdx) = findWord(wordToUse, doc, para, occurrence)
				
				// following the behavior in AnnotationAligner.alignMentionString
				// I will just make a new Mention
				
				// the reason I'm not using AnnotationAligner.alignMentionString here
				// is because that can align to the wrong mention if there is a word
				// repeated in a sentence
				// (note findWord has a similar problem...)
		
				// head token = start because in Roth and Frank data all arguments
				// and predicates are one word
				val (left, right, head) = (tokIdx, tokIdx+1, tokIdx)
				val mention = MentionBuilder.from(sentenceIdx, left, right, head)

				val t = doc.getHeadToken(mention)
				val w = t.getWord
				println("%s\t%s".format(t.getPosTag, Describe.mentionInContext(mention, doc)))
				val pOrA: Either[Predicate, Argument] =
					if(t.getPosTag.startsWith("V") || t.getPosTag.startsWith("JJ") || t.getWord == "decline")
						Left(new Predicate(mention))
					else
						Right(new Argument(mention))
				if(conf != "none")
					storeMentionsIn += new HalfAlignment(pOrA, corefSet, conf == "sure")
				
				true
			}
			case _ => false
		}
	}

}

object RothFrankDocAlignmentReaderTests extends Logging2 {
	import edu.jhu.hlt.parma.diagnostics._
	def main(args: Array[String]) {
		ParmaConfig.load("parma.config")
		teeLogTo(StdOutLogger, new FileLogger("diagnostics/rf-reader.log"))

		val rf = new RothFrankDocAlignmentReader(true, true)
		rf.redirectLogTo(this)
		val (t, das) = Profiler.getTimeAndValue { rf.getDocAlignments }

		log("read %d doc alignments in %.1f seconds".format(das.size, t))
		for((da, idx) <- das.zipWithIndex) {
			log("RF[%d] = %s".format(idx, da.id))
			log("#predSure=%d #predPossible=%d #argSure=%d #argPossible=%d"
				.format(da.surePredicateAlignments.size, da.possiblePredicateAlignments.size,
					da.sureArgCorefAlignments.size, da.possibleArgCorefAlignments.size))

			GeneralDiagnostics.checkDocAlignment(da)

			log("")
			for(a <- da.sureAlignments)
				log(Describe.alignment(a, da.report, da.passage, contextWordsEachSide=10))
			log("\n")
		}

	}
}




