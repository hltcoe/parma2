// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.input

import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.util.AnnotationAligner.HalfAlignment
import no.priv.garshol.duke.comparators.JaroWinkler
import scala.collection.mutable.{Buffer, ArrayBuffer, HashMap}
import scala.collection.JavaConversions._
import scala.util.Random
import scala.io.Source
import scala.xml._
import java.io.File

/**
 * this class reads in and merges:
 * 1) annotated-gigaword pipeline processed version of EECB
 * 2) the mention file included with EECB
 *
 * Note: you almost certainly don't want to run this as the input
 * to an experiment (or parma on-the-fly in general). Use the EECBConverter
 * to dump the DocAlignments to Concrete (Discourse, Communication)
 * protobuf files.
 */
object EECBDocAlignmentReader extends DocAlignmentReader[ParametricDocAlignment[RichConcreteDocBuilder]] with Logging2 {

	private[this] val debug = true

	override def domain = "EECB"

	// functions for going from "13_8.eecb" => (13, 8)
	def getEECBIds(d: RichConcreteDocBuilder): (Int, Int) = {
		val td = d.id.split("\\.")(0)
		val tdi = td.split("_").map(_.toInt)
		(tdi(0), tdi(1))
	}
	def getTopicId(d: RichConcreteDocBuilder): Int = getEECBIds(d)._1
	def getDocId(d: RichConcreteDocBuilder): Int = getEECBIds(d)._2

	class Context(val left: String, val right: String)

	class Candidate[T](val text: String, val context: Context, val passenger: T) {
		//import no.priv.garshol.duke.comparators.JaroWinkler
		import edu.jhu.hlt.parma.features.generic.SimilarityImplementation.levenshteinDistance
		def left = context.left.trim
		def right = context.right.trim
		def similarity(other: Candidate[_]): Double = {

			// how many chars on the left and right to look at?
			val chars = 20
			
			val dt = levenshteinDistance(text, other.text)
			val dl = levenshteinDistance(left.takeRight(chars), other.left.takeRight(chars))
			val dr = levenshteinDistance(right.take(chars), other.right.take(chars))

			// there are some cases of very long NPs, which tend to be appositives
			// "Michael Jordan, famous NBA star and failed baseball player, ..."
			// in these cases, we want to prefer similarty in the beginning of the mention so that
			// if we are limited by the length of the possible candidates we are considering we
			// will choose candidates that match the first part of the appositive
			val s = 50
			val dth =
				if(text.size > s || other.text.length > s)
					// note, use drop not take b/c this is distance;
					// we want to encourage *similarity* at the beginning,
					// so we penalize *distance* at the end of the strings
					2*levenshteinDistance(text.take(s), other.text.take(s))
				else 0

			-2*dt - dl - dr - dth
		}
		override def toString: String = "(Candidate \"%s\" left=\"%s\" right=\"%s\")".format(text, context.left, context.right)
	}

	/**
	 * given a left and right boundary, and assuming this is an NP
	 * find the head word (token index) and make a Mention
	 *
	 * TODO this doesn't handle dependent clauses
	 * e.g. "Man who doesn't like [cheese]" instead of [Man]
	 */
	def chooseHeadEntity(cand: Candidate[(Int, Int)], sentence: Sentence): Mention = {
		val debug = true
		def helper(toks: Seq[Token]): Token = {
			assert(toks.size > 0)
			if(toks.size == 1)
				return toks(0)
			if(debug) println("[chooseHeadEntity] helper toks = " + toks.map(t => t.getWord + ":" + t.getPosTag).mkString(" "))
			
			// if there is a comma, assume that this is an appositive
			// since this is only called on entities (verbs are assumed to be width 1, head is obvious)
			val commaIdx = toks.indexWhere(_.getWord == ",")
			if(commaIdx >= 0) {
				if(commaIdx == 0)
					throw new RuntimeException("toks = " + toks.map(t => t.getWord + ":" + t.getPosTag).mkString(" "))
				return helper(toks.take(commaIdx))
			}

			// if there are any prepositions, start with the stuff on the left
			// note >0 b/c prepositions sometimes come at the begining like "at least 2 million dollars"
			val prepIdx = toks.indexWhere(_.getPosTag == "IN")
			if(prepIdx > 0)
				return helper(toks.take(prepIdx))

			// take the rightmost noun
			var head = toks.size - 1
			while(!toks(head).getPosTag.startsWith("N")) {
				if(head == 0) {
					//throw new RuntimeException("toks = " + toks.map(t => t.getWord + ":" + t.getPosTag).mkString(" "))
					println("found an NP without a noun! " + toks.map(t => t.getWord + ":" + t.getPosTag).mkString(" "))
					return toks.last
				}
				head = head - 1
			}
			toks(head)
		}
		val (left, right) = cand.passenger
		val toks = sentence.tokens.slice(left, right)
		val head = helper(toks)
		MentionBuilder.from(sentence, left, right, head.index)
	}

	def chooseHeadEvent(cand: Candidate[(Int, Int)], sentence: Sentence): Option[Mention] = {
		val (left, right) = cand.passenger
		if(left+1 == right) Some(MentionBuilder.from(sentence, left, right, left))
		else {
			// see if there is exactly one verb, if so it must be what we want
			val toks = sentence.tokens.slice(left, right)
			val vt = toks.filter(_.getPosTag.startsWith("V"))
			if(vt.size == 1)
				Some(MentionBuilder.from(sentence, left, right, vt.head.index))
			else None	// TODO
		}
	}

	/**
	 * extracts all possible candidates from a sentence
	 */
	def findCandidates(sent: Sentence, maxToks: Int = 20): Seq[Candidate[(Int, Int)]] = {
		val toks = sent.tokens
		val k = 10	// how many tokens on each side, used in cases with >1 string identical mentions
		val spans = new ArrayBuffer[Candidate[(Int, Int)]]
		for(start <- 0 until sent.size) {
			for(end <- (start+1) to math.min(start + maxToks, sent.size)) {
				val text = toks.slice(start, end).map(_.getWord).mkString(" ")
				val left = toks.take(start).takeRight(k).map(_.getWord).mkString(" ")
				val right = toks.drop(end).take(k).map(_.getWord).mkString(" ")
				spans += new Candidate[(Int, Int)](text, new Context(left, right), (start, end))
			}   
		}
		spans.toSeq
	}

	/**
	 * recursive function that scans through EECB XML, identifies entities and events,
	 * and then delegates to find the closest mention in the Concrete sentence
	 */
	def findMentions(xml: Node, context: Context, possible: Seq[Candidate[(Int, Int)]], sentence: Sentence, addTo: ArrayBuffer[HalfAlignment[Argument]]) {
		val verbose = true
		val veryVerbose = false
		val left = new StringBuilder(context.left)
		var right: Seq[String] = xml.child.map(_.text)
		for(node <- xml.child) {
			right = right.tail
			val event = node.label == "EVENT"
			val entity = node.label == "ENTITY"
			if(event || entity) {
				val corefId: String = node.attribute("COREFID").get.head.text
				if(!corefId.contains("*")) {
					// choose a mention for this pred/arg
					val smallContext = new Context(left.toString, right.mkString + context.right)
					val target = new Candidate(node.text, smallContext, None)
					if(verbose) println("[findMentions] target = " + target)
					val bestCand = possible.maxBy(_.similarity(target))

					if(veryVerbose) println(possible.sortBy(_.similarity(target)).reverse.take(5).mkString("\n"))

					if(entity) {
						val mention = chooseHeadEntity(bestCand, sentence)
						if(verbose) println("[findMentions] matched to Argument: " + Describe.mentionInContext(mention, sentence, 3))
						addTo += new HalfAlignment(Right(new Argument(mention)), corefId, true)
						findMentions(node, smallContext, possible, sentence, addTo)
					} else {
						chooseHeadEvent(bestCand, sentence) match {
							case Some(mention) =>
								if(verbose) println("[findMentions] matched to Predicate: " + Describe.mentionInContext(mention, sentence, 3))
								addTo += new HalfAlignment(Left(new Predicate(mention)), corefId, true)
							case None =>
								if(verbose) println("[findMentions] skipping \"%s\" because I couldn't identify a clear verbal head".format(node.text))
						}
					}
				}
				else println("skipping \"%s\" because it is a split mention".format(node.text))
			}
			left.append(node.text)
		}
	}

	def fromXML(line: String, sentence: Sentence, addTo: ArrayBuffer[HalfAlignment[Argument]]) {
		val xml = XML.loadString("<S>" + line.replace("&", "&amp;") + "</S>") 
		val candidates = findCandidates(sentence)
		findMentions(xml, new Context("", ""), candidates, sentence, addTo)
	}

	/**
	 * go through the XML, with inline entities and events,
	 * find the nearest constituents to the strings from the inline annotations,
	 * and return them as a sequence of EECBMentions
	 */
	def getMentionsFromInlineAnnotations(f: File, doc: RichConcreteDocBuilder): Seq[HalfAlignment[Argument]] = {
		val debug = false
		if(debug)
			println("[getMentionsFromInlineAnnotations] matching up %s and %s".format(doc.id, f.getPath))
		val mentions = new ArrayBuffer[HalfAlignment[Argument]]
		var sentence = 0
		val br = FileUtils.getReader(f)
		while(br.ready) {
			val line = br.readLine
			fromXML(line, doc.sentences(sentence), mentions)
			sentence = sentence + 1
		}
		br.close
		if(debug) {
			for((ha, idx) <- mentions.zipWithIndex)
				println("[getMentionsFromInlineAnnotations] ha(%d) = %s".format(idx, Describe.halfAlignment(ha, doc)))
		}
		mentions.toSeq
	}

	def getAllMentionsFromInlineAnnotations(dir: File, docs: Seq[RichConcreteDocBuilder]): Seq[HalfAlignmentWithLocation] = {
		// the set gets rid of duplicate HalfAlignments
		// (same same mention, different coref sets -- TODO check this)
		val allMentions = new collection.mutable.HashSet[HalfAlignmentWithLocation]
		for(d <- docs) {
			val ti = getTopicId(d)
			val di = getDocId(d)
			val f = new File(dir, "%d/%d.eecb".format(ti, di))
			assert(f.exists && f.isFile, "f = " + f.getPath)
			allMentions ++= getMentionsFromInlineAnnotations(f, d).map(ha =>
				new HalfAlignmentWithLocation(ha, ti, di))
		}
		allMentions.toSeq
	}

	class HalfAlignmentWithLocation(val halfAlignment: HalfAlignment[Argument], val topicId: Int, val docId: Int)

	override def getDocAlignments: Seq[ParametricDocAlignment[RichConcreteDocBuilder]] = {

		val debug = false

		val docs = ConcreteWrapper.getCommunicationsFrom(ParmaConfig.getDirectory("data.eecb.concrete.documents"))
			.map(new RichConcreteDocBuilder(_))
		val inlineDir = ParmaConfig.getFile("data.eecb.annotations.inline")	// e.g. data/eecb/docs/raw/
		val mentions = getAllMentionsFromInlineAnnotations(inlineDir, docs)

		// put mentions in map by topic and doc ids
		val halfAlignmentsByTopicAndDoc: Map[Int, Map[Int, Seq[HalfAlignment[Argument]]]] =
			mentions.groupBy(_.topicId).mapValues(_.groupBy(_.docId).mapValues(_.map(_.halfAlignment)))

		if(debug) {
			for((t, m) <- halfAlignmentsByTopicAndDoc)
				for((d, sha) <- m)
					println("[getAlignedDocuments] topic=%s doc=%s has=%s".format(t, d, sha.mkString(", ")))
		}

		val das = new ArrayBuffer[ParametricDocAlignment[RichConcreteDocBuilder]]
		for((topicId, tdocs) <- docs.groupBy(d => getTopicId(d))) {

			// choose the first id as the report, rest are passages
			val tdocss = tdocs.sortBy(getDocId(_))
			val reportOrig = tdocss.head
			val passages = tdocss.tail

			// half-alignments in the report
			//val reportHAs = mentionsByTopicAndDoc(topicId)(getDocId(reportOrig)).map(_.toHalfAlignment(reportOrig))
			if(debug) println("[getAlignedDocuments] report loop topidId=%d docId=%d doc=%s".format(topicId, getDocId(reportOrig), reportOrig.id))
			val reportHAs = halfAlignmentsByTopicAndDoc(topicId)(getDocId(reportOrig))
			if(debug) {
				println("reportHAs = ")
				println(reportHAs.mkString("\n"))
				println()
			}

			// NOTE: need to copy report because makeDocAlignment
			// adds preds/args each time. if there are N passages,
			// each pred/arg in report will be added N times instead of 1

			for(passage <- passages) {

				val report = reportOrig.deepCopy().asInstanceOf[RichConcreteDocBuilder]

				// half-alignments in the passage
				//val passageHAs = mentionsByTopicAndDoc(topicId)(getDocId(passage)).map(_.toHalfAlignment(passage))
				if(debug) println("[getAlignedDocuments] passage loop topidId=%d docId=%d doc=%s".format(topicId, getDocId(passage), passage.id))
				val passageHAs = halfAlignmentsByTopicAndDoc(topicId)(getDocId(passage))
				if(debug) {
					println("passageHAs = ")
					println(passageHAs.mkString("\n"))
					println()
				}

				das += AnnotationAligner.makeDocAlignment(
					report, reportHAs, passage, passageHAs, Some(domain), addPredArgs=true, strict=false)
			}
		}
		das.toSeq
	}
}

