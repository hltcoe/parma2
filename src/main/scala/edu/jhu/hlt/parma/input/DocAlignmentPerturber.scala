// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.input

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.inference.{ DocMetaAligner, CanonicalMentionFinder }
import edu.jhu.hlt.parma.diagnostics.GeneralDiagnostics
import scala.util.Random
import scala.collection.JavaConversions._
import scala.collection.mutable.{ArrayBuffer, HashSet}

/**
 * the purpose of this code is to make negative examples for pred-arg alignment
 */
object DocAlignmentPerturber extends Logging2 {

	private var rand = new Random(9001)
	def setSeed(seed: Int) { rand = new Random(seed) }

	sealed class SubIn(val sentence: Sentence,
					val preds: Seq[Predicate],
					val args: Seq[Argument])

	private def chooseIncoming(howManySentences: Int, source: Seq[Document]): Seq[SubIn] = {
		// choose one sentence per document
		val subIns = new ArrayBuffer[SubIn]
		for(d <- rand.shuffle(source.toBuffer).take(howManySentences)) {
			val sentIdx = rand.nextInt(d.sentences.size)
			val sent: Sentence = d.sentences(sentIdx)
			val preds = d.predicates.filter(_.location.getSentenceIdx == sentIdx)
			val args = d.arguments.filter(_.location.getSentenceIdx == sentIdx)
			val anonPreds = preds.map(p => new Predicate(p.location))
			val anonArgs = args.map(a => new Argument(a.location))
			subIns += new SubIn(sent, anonPreds, anonArgs)
		}
		subIns
	}

	private def changeMentionSentence(newSentIdx: Int, m: Mention): Mention =
		MentionBuilder.from(newSentIdx, m.getStartTokenIdx, m.getEndTokenIdx, m.getHeadTokenIdx)

	/**
	 * swaps in sentences (and preds/args) into this document
	 */
	private def modifyDocument(doc: DocumentBuilder, targetSentences: Seq[Int],
			substitutions: Seq[SubIn], propRemove: Double): DocumentBuilder = {

		assert(targetSentences.size == substitutions.size)

		val newDoc = doc.deepCopy
		assert(newDoc.predicates.size + newDoc.arguments.size + newDoc.corefs.size == 0,
			"newDoc should be empty (need to be adding to empty document)")

		// remove preds/args in targetSentences
		val predsKeep = doc.predicates.filterNot(pred =>
			targetSentences.contains(pred.location.getSentenceIdx))
		val argsKeep = doc.arguments.filterNot(arg =>
			targetSentences.contains(arg.location.getSentenceIdx))
		for(pred <- predsKeep)
			newDoc.addPredicate(pred)
		for(arg <- argsKeep)
			newDoc.addArgument(arg)

		// add preds/args from subIns
		// swap in Sentences from subIns
		for((targetIdx, subIn) <- targetSentences.zip(substitutions)) {

			newDoc.setSentence(targetIdx, subIn.sentence)

			// need to make sure all these preds/args have locations which point to sentenceIdx=targetIdx
			if(rand.nextDouble > propRemove) {

				// move all of the predicates and arguments to the new document
				for(pred <- subIn.preds) {
					val movedPred = new Predicate(changeMentionSentence(targetIdx, pred.location))
					newDoc.addPredicate(movedPred)
				}
				for(arg <- subIn.args) {
					val movedArg = new Argument(changeMentionSentence(targetIdx, arg.location))
					newDoc.addArgument(movedArg)
				}

			}
		}

		// for testing
		for(arg <- argsKeep) log("[modifyDocument] argsKeep=" + Describe.argument(arg, newDoc))
		for(pred <- predsKeep) log("[modifyDocument] predsKeep=" + Describe.predicate(pred, newDoc))

		warnIf(newDoc.predicates.size == 0,
			"the new document, %s, doesn't have any predicates!".format(newDoc.id))
		warnIf(newDoc.arguments.size + newDoc.corefs.size == 0,
			"the new document, %s, doesn't have any arguments!".format(newDoc.id))

		newDoc
	}

	/**
	 * removes alignments that touched a pred/arg that has been removed
	 */
	private def modifyAlignment[D <: DocumentBuilder](da: ParametricDocAlignment[D],
			newPassage: D, targetSentences: Seq[Int]): ParametricDocAlignment[D] = {

		val verbose = true

		val zombieSents: Set[Int] = targetSentences.toSet
		if(verbose) log("[modifyAlignments] zombieSEnts.size = " + zombieSents.size)
		def inZombieSentence(a: Alignment): Boolean = a match {
			case pa: PredicateAlignment =>
				zombieSents.contains(pa.passagePred.location.getSentenceIdx)
			case aca: ArgCorefAlignment =>
				val ps: Set[Int] = aca.passageCoref.map(_.location.getSentenceIdx).toSet
				(zombieSents & ps).size > 0
		}

		val skeep = da.sureAlignments.filterNot(inZombieSentence)
		val pkeep = da.possibleAlignments.filterNot(inZombieSentence)
		if(verbose) {
			log("[modifyAlignment] da.sure %d => %d, da.possible %d => %d"
				.format(da.sureAlignments.size, skeep.size, da.possibleAlignments.size, pkeep.size))
		}
		val id = "r%s_p%s-MODIFIED".format(da.report.id, newPassage.id)
		val newDA = new ParametricDocAlignment[D](id, da.domain, da.report, newPassage, skeep, pkeep)
		GeneralDiagnostics.checkDocAlignment(newDA)

		if(verbose) {
			for(a <- newDA.sureAlignments)
				log("[modifyAlignment] sure=" + Describe.alignment(a, newDA.report, newDA.passage))
			for(a <- newDA.possibleAlignments)
				log("[modifyAlignment] possible=" + Describe.alignment(a, newDA.report, newDA.passage))
		}

		newDA
	}

	/**
	 * propSub of the sentences in passage will be swapped out for a random sentence
	 * propRemove of the swapped sentences will have their Arguments and Predicates dropped
	 * (fewer Arguments/Predicates makes for faster inference)
	 */
	def degradeDocAlignment(
				da: ParametricDocAlignment[DocumentBuilder],
				outOfDomain: Seq[Document],
				propSub: Double = 0.5,
				propRemove: Double = 0d): ParametricDocAlignment[DocumentBuilder] = {

		val n = da.passage.sentences.size
		val howManySents = math.ceil(n * propSub).toInt
		val subIns = chooseIncoming(howManySents, outOfDomain)
		val targetSentences = rand.shuffle((0 until n).toBuffer).take(howManySents)
		val newPassage = modifyDocument(da.passage, targetSentences, subIns, propRemove)
		println("[degradeDocAlignment] da.report=%s da.passage=%s newPassage=%s"
			.format(da.report.id, da.passage.id, newPassage.id))
		val newDA = modifyAlignment[DocumentBuilder](da, newPassage, targetSentences)

		newDA
	}



	// TODO everything below this <<<<<<<<<<<<<<<<<<<<<<<<<<<
	// should be moved to another class
	// below this is used to select a subset, not perturb a doc alignment

	
	def leastOverlapSubset[DA <: DocAlignment](alignments: Seq[DA], howMany: Int): Seq[DA] = {
		val sorted = alignments.sortBy(overlappiness)
		
		val k = 20
		log("[DocAlignmentPerturber leastOverlapSubset] taking %d of %d doc alignments"
			.format(howMany, alignments.size))
		log("[DocAlignmentPerturber leastOverlapSubset] least-overlappy %d alignments:".format(k))
		for((da, idx) <- sorted.take(k).zipWithIndex)
			log("[DocAlignmentPerturber leastOverlapSubset] overlappy=%.3g, da(%d)=%s"
				.format(overlappiness(da), idx, Describe.docAlignment(da)))
		log("[DocAlignmentPerturber leastOverlapSubset] most-overlappy %d alignments:".format(k))
		for((da, idx) <- sorted.reverse.take(k).zipWithIndex)
			log("[DocAlignmentPerturber leastOverlapSubset] overlappy=%.3g, da(%d)=%s"
				.format(overlappiness(da), idx, Describe.docAlignment(da)))
			
		val as = sorted.take(howMany)
		log("[DocAlignmentPerturber leastOverlapSubset] overlappiness of keep: " +
			as.map(overlappiness).mkString(", "))
		log("[DocAlignmentPerturber leastOverlapSubset] overlappiness of everything: " +
			sorted.map(overlappiness).mkString(", "))
		
		as
	}
		
	/**
	 * include DocAlignments that have lexical-overlap no higher than overlapCutoff
	 */
	def leastOverlapSubset[DA <: DocAlignment](alignments: Seq[DA], overlapCutoff: Double): Seq[DA] =
		alignments.filter(da => overlappiness(da) <= overlapCutoff)
	
	def overlappiness(da: DocAlignment): Double = {
		val sure = da.sureAlignments.toBuffer		// don't use set.map because it will collapse repeats
		val possible = da.possibleAlignments.toBuffer
		val s = sure.map(a => overlappiness(a, da.context)).sum
		val p = possible.map(a => overlappiness(a, da.context)).sum
		val trustSure = 0.75d
		trustSure * (s / sure.size) + (1d - trustSure) * (p / possible.size)
	}
	
	def overlappiness(a: Alignment, c: Context): Double = {
		val (rCM, pCM) = CanonicalMentionFinder.canonicalMentions(a, c)
		val rt = c.report.getHeadToken(rCM)
		val pt = c.passage.getHeadToken(pCM)
		val lemma = if(rt.getLemma equalsIgnoreCase pt.getLemma) 1d else 0d
		val word = if(rt.getWord equalsIgnoreCase pt.getWord) 1d else 0d
		val pos = if(rt.getPosTag equalsIgnoreCase pt.getPosTag) 1d else 0d
		(lemma + 0.1d*word + 0.05d*pos) / 1.15d
	}
	
}

