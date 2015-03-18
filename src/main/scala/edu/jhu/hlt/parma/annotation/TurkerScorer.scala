// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.annotation

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.input.RothFrankDocAlignmentReader
import edu.jhu.hlt.parma.inference._
import edu.jhu.hlt.parma.math.Stats
import edu.jhu.hlt.concrete.Concrete.Communication
import edu.jhu.hlt.parma.util.AlignmentFileUtil.AlignmentRef
import java.io._
import collection.Map
import collection.mutable.{ ArrayBuffer, HashMap, HashSet }
import collection.JavaConversions._

// TODO move this out into its own file
trait AlignmentScorer {
	def score(a: Alignment, report: Document, passage: Document, domain: Option[String]): Double
	def score(a: Alignment, c: Context): Double = score(a, c.report, c.passage, None)
}

/**
 * special made for Chris' format
 */
object AltTurkerScorer {

	/** @deprecated use MentionFileUtils.MentionRef instead */
	class TurkMention(val doc: Communication, val mention: Mention, val isPredicate: Boolean)

	def main(args: Array[String]) {
		if(args.length < 5 || args.length > 6) {
			println("please provide:")
			println("1) a turker's mentions TSV (assignmentId, source_mention, target_mention1 ... target_mention_n)")
			println("2) a .mentions file")
			println("3) a concrete communications file (e.g. from the anno pipeline + concrete-agiga)")
			println("4) a serialized model file")
			println("5) an output file")
			println("6) optional: a parma.config file (defaults to Travis')")
			return
		}

		val mrlFile = new File(args(0))
		val mentionsFile = new File(args(1))
		val communicationsFile = new File(args(2))
		val modelFile = new File(args(3))
		val outfile = new File(args(4))
		ParmaConfig.load(
			if(args.length == 6) args(5)
			else "/home/hltcoe/twolfe/miniScale2013/parma/parma.config"
		)

		println("reading model in from " + modelFile.getPath)
		val ois = new ObjectInputStream(new FileInputStream(modelFile))
		val model = ois.readObject.asInstanceOf[HierarchicalAlignmentModule]
		model.preTrainCalibrate(Seq())	// setup feature functions, etc
		ois.close


		val mrlStrs = readMRLs(mrlFile)
		val docs = ConcreteWrapper.getCommunicationsFrom(communicationsFile).map(d => (d.getGuid.getCommunicationId, d)).toMap
		val mentions = readMentions(mentionsFile, docs)
		//assert(mentions.contains("APW_ENG_20040717.0096-a0.0"))
		//println(scala.util.Random.shuffle(mentions.keys.take(30)))
		val mrls = convertMRLs(mrlStrs, mentions)
		emitScores(outfile, model, mrls)
	}

	/** @deprecated use MentionFileUtil */
	def readMentions(f: File, docMap: Map[String, Communication]): Map[String, TurkMention] = {
		println("reading mentions from " + f.getPath)
		val mmap = new HashMap[String, TurkMention]
		val TurkMentionLine = """(\S+)\t(\S+)\t(\S+)\t(\d+)\t(\d+)\t(\d+)\t(\d+)\t(\S+)""".r
		val br = FileUtils.getReader(f)
		while(br.ready) {
			val line = br.readLine
			if(!line.startsWith("#")) {
				line match {
					case TurkMentionLine(predArgId, typ, docId, sentIdxStr, startIdxStr, endIdxStr, headIdxStr, word) =>
						val doc = docMap(docId)
						val mention = MentionBuilder.from(sentIdxStr.toInt, startIdxStr.toInt, endIdxStr.toInt, headIdxStr.toInt)
						// always mark as predicate because that is how the RF reader handles this now
						mmap += (predArgId -> new TurkMention(doc, mention, true))
						//mmap += (predArgId -> new TurkMention(doc, mention, typ equalsIgnoreCase("predicate")))
					case _ =>
						throw new RuntimeException("i dont get this line: " + line)
				}
			}
		}
		br.close
		println("loaded %d mentions from %s".format(mmap.size, f.getPath))
		mmap
	}

	def convertMRLs(in: Seq[MentionRankLine[String]], mentionMap: Map[String, TurkMention]): Seq[MentionRankLine[(String, TurkMention)]] = {
		//in.map(_.map((s: String) => (s, mentionMap(s))))
		println("converting %d string MRLs...".format(in.size))
		val out = new ArrayBuffer[MentionRankLine[(String, TurkMention)]]
		for(mrls <- in) {
			//println("souceMention = " + mrls.sourceMention)
			//println("targetMentios = " + mrls.targetMentions)
			val sid = mentionMap(mrls.sourceMention)
			val tids = mrls.targetMentions.map(tid => mentionMap(tid))
			out += new MentionRankLine[(String, TurkMention)](mrls.assignmentId, (mrls.sourceMention, sid), mrls.targetMentions.zip(tids))
		}
		out.toSeq
	}

	def emitScores(f: File, model: HierarchicalAlignmentModule, mrl: Seq[MentionRankLine[(String, TurkMention)]]) {
		println("about to emit %d MRLs...".format(mrl.size))
		val w = FileUtils.getWriter(f)
		for((m, idx) <- mrl.zipWithIndex) {
			if(idx % 100 == 0) print("*")
			w.write(scoreMRL(model, m) + " " + model.getThreshold + "\n")
		}
		println("")
		w.close
	}
	
	def scoreMRL(model: AlignmentScorer, mrl: MentionRankLine[(String, TurkMention)]): String = {
		val sp = new Predicate(mrl.sourceMention._2.mention)
		val report = new RichConcreteDoc(mrl.sourceMention._2.doc)
		val scores = mrl.targetMentions.map(tm => {
			val a = new PredicateAlignment(sp, new Predicate(tm._2.mention))
			val passage = new RichConcreteDoc(tm._2.doc)
			model.score(a, new Context(report, passage))
		})
		"%s\t%s\t%s".format(mrl.assignmentId, mrl.sourceMention._1, scores.mkString(" "))
	}

	class MentionRankLine[T](
		val assignmentId: String,
		val sourceMention: T,
		val targetMentions: Seq[T]) {
		def map[R](f: T => R): MentionRankLine[R] = new MentionRankLine(assignmentId, f(sourceMention), targetMentions.map(f))
		override def toString: String = "%s\t%s\t%s".format(assignmentId, sourceMention, targetMentions.mkString(" "))
	}

	private def readMRLs(f: File): Seq[MentionRankLine[String]] = {
		val br = FileUtils.getReader(f)
		val mls = new ArrayBuffer[MentionRankLine[String]]
		while(br.ready) {
			val ar = br.readLine.split("\\t")
			assert(ar.length >= 3, "ar = " + ar)
			val aid = ar(0).trim
			val sid = ar(1).trim
			val tids = ar.slice(2, ar.length).map(_.trim)
			mls += new MentionRankLine(aid, sid, tids)
		}
		br.close
		mls.toSeq
	}
}

/**
 * the idea is to run parma on alignments produce by
 * turkers and give a score of some sort. if this score
 * is much lower than gold standard annotations or other
 * turkers then that turker is not to be trusted
 */
object TurkerScorer {

	// description that goes into DocAlignments, providence information
	val domain = "TurkScores-F1"	// TODO make this specific to one turker

	def main(args: Array[String]) {
		if(args.length < 4 || args.length > 5) {
			println("please provide:")
			println("1) a .alignment file")
			println("2) a .mentions file")
			println("3) a concrete communications file (e.g. from the anno pipeline + concrete-agiga)")
			println("4) a serialized model file")
			println("5) optional: a parma.config file (defaults to Travis')")
			return
		}

		val useF1 = true // alternative is log-likelihood

		val alignmentFile = new File(args(0))
		val mentionsFile = new File(args(1))
		val communicationsFile = new File(args(2))
		val modelFile = new File(args(3))
		ParmaConfig.load(
			if(args.length == 5) args(4)
			else "/home/hltcoe/twolfe/miniScale2013/parma/parma.config"
		)

		val docAlignments = HITIngester.readAlignments(communicationsFile, mentionsFile, alignmentFile)

		println("reading model in from " + modelFile.getPath)
		val ois = new ObjectInputStream(new FileInputStream(modelFile))
		val model = ois.readObject.asInstanceOf[InferenceEngine[_]]
		model.preTrainCalibrate(Seq())	// setup feature functions, etc
		ois.close

		// group alignments by assignmentId,
		// foreach make a DocAlignment (requires looking up mentions => preds/args)
		// score the DocAlignment with HAM
		val scores = new ArrayBuffer[Double]
		val guessing_scores = new ArrayBuffer[Double]
		val f1Scores = new ArrayBuffer[Double]
		for(da <- docAlignments) {

			//val rfDA = RFAlignments.alignmentMatching(da)
			val parmaDA = model.align(da.report, da.passage, Some(domain))
			//f1Scores += compareTurkerToGold(da, rfDA)
			f1Scores += compareTurkerToGold(da, parmaDA)
			scores += model.score(da, Some(domain))
			guessing_scores += logLikelihoodUnderRandomGuessing(da)
		}

		import scala.util.Random
		val nSample = 20
		for((s, name) <- Seq((scores, "model-scores"), (guessing_scores, "guessing-scores"))) {
			println("\n======== %s ========".format(name))
			println("scores = " + Random.shuffle(s).take(nSample))
			println("mean = " + Stats.mean(s, skipNaN=true))
			println("std dev = " + Stats.stdDev(s, skipNaN=true))
			println("min = " + s.min)
			println("max = " + s.max)
			println("num assignments = " + s.size)
		}
		println("\n======== turker-parma F1s ========")
		println("scores = " + Random.shuffle(f1Scores).take(nSample))
		println("mean = " + Stats.mean(f1Scores, skipNaN=true))
		println("std dev = " + Stats.stdDev(f1Scores, skipNaN=true))
		println("min = " + f1Scores.min)
		println("max = " + f1Scores.max)
		println("num assignments = " + f1Scores.size)

	}

	// NOTE: this is not what we want
	// we cannot try to align mentions that come from parma's PredArgSelector
	// with mentions that come from the RF dataset -- they wont match up
	object RFAlignments {
		import edu.jhu.hlt.parma.input.ConcreteDocAlignmentReader
		val docAlignments = ConcreteDocAlignmentReader.RF.getDocAlignments
		def alignmentMatching(turkerAlignment: DocAlignment): DocAlignment = {
			val try1 = docAlignments.filter(da =>
				da.report.asInstanceOf[CommunicationDocument].communication.getGuid.getCommunicationId == turkerAlignment.report.id &&
				da.passage.asInstanceOf[CommunicationDocument].communication.getGuid.getCommunicationId == turkerAlignment.passage.id)
			if(try1.size == 1)
				return try1.head
			else if(try1.size > 1)
				throw new RuntimeException("wat")
			else {
				val try2 = docAlignments.filter(da =>
					da.passage.asInstanceOf[CommunicationDocument].communication.getGuid.getCommunicationId == turkerAlignment.report.id &&
					da.report.asInstanceOf[CommunicationDocument].communication.getGuid.getCommunicationId == turkerAlignment.passage.id)
				if(try2.size == 1)
					return try2.head
				if(try2.size == 0) {
					println("cannot look up RF doc pair for: report=%s passage=%s"
						.format(turkerAlignment.report.id, turkerAlignment.passage.id))
					throw new java.util.NoSuchElementException
				}
				else
					throw new RuntimeException("wat: " + try2.size)
			}
		}
	}

	// returns F1
	private def compareTurkerToGold(turker: DocAlignment, gold: DocAlignment): Double = {
		val goldSureAdjusted = DocMetaAligner.allPossibleAlignments(turker.report, turker.passage).toSet & gold.sureAlignments
		val intersection = turker.possibleAlignments & goldSureAdjusted
		if(intersection.size == 0 && gold.sureAlignments.size > 0 && turker.sureAlignments.size > 0) {
			println("intersection.size == 0")
			for(a <- gold.sureAlignments)
				println("gold: " + Describe.alignment(a, gold.context.report, gold.context.passage))
			for(a <- turker.sureAlignments)
				println("turker: " + Describe.alignment(a, turker.context.report, turker.context.passage))
		}
		val p = intersection.size.toDouble / turker.sureAlignments.size
		val r = intersection.size.toDouble / goldSureAdjusted.size
		if(p + r == 0d) 0d
		else 2d*p*r / (p+r)
	}


	/**
	 * this is not what i want, this is the likelihood of the guesses given parameters
	 * estimated from the guesses... i.e. the likelihood in MLE
	 */
	private def logLikelihoodUnderRandomGuessing(da: DocAlignment): Double = {
		val pos = da.sureAlignments.size
		val n = DocMetaAligner.allPossibleAlignments(da.report, da.passage).size
		val neg = n - pos
		//val eps = 1e-10
		//val p =
		//	if(pos == 0) eps
		//	else if(neg == 0) 1d-eps
		//	else pos.toDouble / n
		//val p = 105d / 3353d
		val p = (1877 + 1308).toDouble / 85658
		val logL = pos * math.log(p) + neg * math.log(1d - p)
		val r = logL / n
		println("[random guessing] pos=%d neg=%s p=%.3f r=%.3f".format(pos, neg, p, r))
		r
	}
}

