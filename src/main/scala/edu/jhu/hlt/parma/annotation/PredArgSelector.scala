// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.annotation

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.features.ReportingVerbs
import edu.jhu.hlt.parma.util._
import java.io.File
import scala.collection.mutable.{ HashSet, Buffer, ArrayBuffer }
import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap

/**
 * @deprecated use TimeSieve instead
 * identifies predicates as
 *     union(Nate Chambers' event detector, verbs) - stopwords
 * identifies arguments as:
 *     mode = "dep":  all noun tokens that are 1 hop away in a dep parse
 *     mode = "head": all heads of NPs (uses Stanford's semantic head finder)
 *     mode = "pos":  all nouns
 */
object NatesPreds {
	
	var mode = "dep"
	
	// must contain run-eventclassifier.sh and chambers-jars/
	var natesCodePath = new File("/home/hltcoe/twolfe/miniScale2013/nate-predicates/from-nate/")

	def main(args: Array[String]) {
		if(args.length < 5 || args.length > 6) {
			println("please provide:")
			println("1) a concrete documents file (e.g. from anno pipeline + concrete-agiga)")
			println("2) an output mentions file")
			println("3) a scratch directory")
			println("4) a mode [dep|head|pos]")
			println("5) Nate's event classifier location (should contain run-eventclassifier.sh and chambers.jars/)")
			println("6) optional: a parma.config file (defaults to Travis')")
			return
		}
		val concreteFile = new File(args(0))
		val outputFile = new File(args(1))	//new File("/home/hltcoe/twolfe/miniScale2013/nate-pred-outfile-" + mode)
		val scratch = new File(args(2))
		assert(scratch.exists && scratch.isDirectory, "you must provide an existing directory: " + scratch.getPath)
		val predFile = new File(scratch, "nate-pred-predicates")
		val tempFile = new File(scratch, "nate-pred-tempfile")
		mode = args(3).toLowerCase
		assert(Set("dep", "head", "pos").contains(mode))

		natesCodePath = new File(args(4))

		ParmaConfig.load(
			if(args.length == 6) args(5)
			else "/home/hltcoe/twolfe/miniScale2013/parma/parma.config"
		)

		extractParses(concreteFile, tempFile)

		runNatesExtractor(tempFile, predFile)
		val predicates = readNatesPreds(predFile)

		val docIter = ConcreteWrapper.getCommunicationsFrom(concreteFile).map(new RichConcreteDocBuilder(_)).iterator
		val docs = addPredArgs(docIter, predicates)

		MTurkUtils.dumpMentions(outputFile, docs.toSeq)
	}

	private def addPredArgs(docIter: Iterator[DocumentBuilder], predicates: Buffer[NatePred]): Buffer[DocumentBuilder] = {
		val stopwords = PredArgSelector.stopwordsAPriori
		assert(stopwords.contains("said"))
		val docs = new ArrayBuffer[DocumentBuilder]

		var sentCounter = 0	// number of sentences processed so far
		var pIdx = 0		// index into Nate's predicate output file

		for((doc, docIdx) <- docIter.zipWithIndex) {
			var corefSet = 1
			docs += doc
			println("doc %d has %d sentences".format(docIdx, doc.sentences.size))

			// head tokens, used to exclude nominals that are predicates not arguments
			val nominalPredicates = new HashSet[Token]

			// add predicates
			val natePreds = new HashSet[Token]
			while(pIdx < predicates.size && predicates(pIdx).lineNum < sentCounter + doc.sentences.size) {
				val sentIdx = predicates(pIdx).lineNum - sentCounter
				val pred = predicates(pIdx).convert(sentIdx)
				//println("extracting: "+ predicates(pIdx))
				//println("sentIdx = " + sentIdx)
				//println(Describe.sentence(doc.getSentence(sentIdx)))
				val tok = doc.getHeadToken(pred)
				if(!stopwords.contains(tok.getWord) && !stopwords.contains(tok.getLemma)) {
					natePreds += tok
					doc.addPredicate(pred)//, corefSet.toString)
					corefSet += 1
					if(tok.getPosTag.startsWith("N"))
						nominalPredicates += tok
				}
				pIdx += 1
			}
			sentCounter += doc.sentences.size

			// take any verbs that nate didn't take
			for(sent <- doc.sentences; tok <- sent.tokens) {
				if(tok.getPosTag.startsWith("V") && !natePreds.contains(tok) &&
					!stopwords.contains(tok.getWord) && !stopwords.contains(tok.getLemma)) {
					doc.addPredicate(new Predicate(MentionBuilder.from(sent, tok)))//, corefSet.toString)
					corefSet += 1
				}
			}

			// identify all arguments
			// TODO check if any of these arguments appear in a coref chain
			var skippedArgs = 0
			val args = mode match {
				case "pos" => for(sent <- doc.sentences; arg <- PredArgSelector.potentialArgsByPOS(sent)) yield arg
				case "dep" => for(sent <- doc.sentences; arg <- PredArgSelector.potentialArgsByDeps(sent, doc.predicates)) yield arg
				case "head" => for(sent <- doc.sentences; arg <- PredArgSelector.potentialArgsByHead(sent)) yield arg
			}
			for(arg <- args) {
				val tok = doc.getHeadToken(arg)
				if(!nominalPredicates.contains(tok)) {
					doc.addArgument(arg)//, corefSet.toString)
					corefSet += 1
				}
				else skippedArgs += 1
			}
			println("there were %d nominal predicates in %s, skipped %d nominals that would have been args".format(nominalPredicates.size, doc.id, skippedArgs))
		}
		docs
	}

	private def extractParses(concreteFile: File, dest: File) {
		println("[extractParses] %s => %s".format(concreteFile.getPath, dest.getPath))
		val writer = FileUtils.getWriter(dest)
		for(doc <- ConcreteWrapper.getDocumentsFrom(concreteFile)) {
			for(sent <- doc.sentences) {
				sent.ptbParse match {
					case None =>
						throw new RuntimeException("need ptb parse for this to work!")
					case Some(p) =>
						val sexp = TreeUtils.sExpressionForParse(p)
						writer.write(sexp + "\n")
				}
			}
		}
		writer.close
	}

	// call nates code
	private def runNatesExtractor(parseFile: File, outputFile: File) {
		println("[runNatesExtractor] parseFile=%s outputFile=%s".format(parseFile.getPath, outputFile.getPath))
		import scala.sys.process._
		val script = new File(natesCodePath, "run-eventclassifier.sh")
		val jarDir = new File(natesCodePath, "chambers-jars")
		val logging = Seq(script, jarDir, parseFile).map(_.getPath).!!
		println(logging)
		println(Seq("mv", parseFile.getPath + ".onlyevents", outputFile.getPath).!!)
	}

	case class NatePred(val lineNum: Int, val tokIdx: Int, val word: String, val typ: String, val tense: String) {
		def convert(sentIdx: Int) = new Predicate(MentionBuilder.from(sentIdx, tokIdx))
		override def toString: String = "(NatePred lineNum=%d tok=%d word=%s type=%s tense=%s)".format(lineNum, tokIdx, word, typ, tense)
	}

	def readNatesPreds(f: File): Buffer[NatePred] = {
		val preds = new ArrayBuffer[NatePred]
		val br = FileUtils.getReader(f)
		while(br.ready) {
			val line = br.readLine
			val ar = line.trim.split("\t")
			assert(ar.length == 5)
			val sentIdx = ar(0).toInt // line number ish
			val tokIdx = ar(1).toInt
			val word = ar(2)
			val typ = ar(3)
			val tense = ar(4)
			preds += NatePred(sentIdx, tokIdx, word, typ, tense)
		}
		br.close
		preds
	}

}


/**
 * @deprecated super deprecated, prefer NatesPreds
 */
object PredArgSelector {

	def potentialArgsByHead(sent: Sentence) = {
		val debug = false
		sent.ptbParse match {
			case None => throw new RuntimeException("I can't do anything without a PTB parse!")
			case Some(root) => {
				if(debug) {
					println("[potentialArguments] root.topDownTraversal = " + root.topDownTraversal)
					println("[potentialArguments] NPs = " + root.topDownTraversal.filter(_.value == "NP"))
					println("[potentialArguments] NPs heads = " + root.topDownTraversal.filter(_.value == "NP").map(_.headToken))
				}
				for((tok: Token) <- root.topDownTraversal.filter(_.value == "NP").map(_.headToken).filter(_.getPosTag.startsWith("N")).toSet.toSeq)
					yield new Argument(MentionBuilder.from(sent, tok))
			}
		}
	}

	def potentialArgsByDeps(sent: Sentence, preds: Seq[Predicate]): Seq[Argument] = preds.flatMap(p => potentialArgsByDeps(sent, p)).toSet.toSeq
	def potentialArgsByDeps(sent: Sentence, pred: Predicate): Seq[Argument] = {
		sent.governedBy(pred.location).map(_.dep).filter(_.getPosTag.startsWith("N")).map(t => new Argument(MentionBuilder.from(sent, t)))
	}

	def potentialArgsByPOS(sent: Sentence) = sent.tokens.filter(_.getPosTag.startsWith("N")).map(t => new Argument(MentionBuilder.from(sent, t)))

	/**
	 * this doesn't try to align with coref (yet)
	 *
	 * TODO add support for general Document as input
	 */
	def identifyPredicatesAndArguments(doc: DocumentBuilder): DocumentBuilder = {

		val verbose = true
		
		val newDoc = doc.deepCopy

		// ==== PREDICATES ====
		val sw = stopwordsAPriori
		for((sent, sentIdx) <- doc.sentences.zipWithIndex) {
			for((tok, tokIdx) <- sent.tokens.zipWithIndex) {
				if(!sw.contains(tok.getLemma.toLowerCase)
						&& !sw.contains(tok.getWord.toLowerCase)
						&& tok.getPosTag.startsWith("V")) {
					val startTokIdx = tokIdx
					val headTokIdx = tokIdx
					val endTokIdx = tokIdx + 1
					val m = MentionBuilder.from(sentIdx, startTokIdx, endTokIdx, headTokIdx)
					newDoc.addPredicate(new Predicate(m))
				}
			}
		}
		
		// ==== ARGUMENTS ====
		// assume every coref mention is an argument
		val MAX_TOKENS = 8	// no more than this many tokens in an Argument
		if(doc.corefs.size > 0) {
			if(verbose)
				println("[identifyPredicatesAndArguments] identifying arguments by looking at existing coref chains")
			for((coref, corefIdx) <- doc.corefs.zipWithIndex)
				for(arg <- coref)
					if(arg.location.width <= MAX_TOKENS)
						newDoc.addArgument(arg)//, corefIdx.toString)
		} else {
			// fill out arguments by looking for Nouns that are governed by verbs
			if(verbose)
				println("[identifyPredicatesAndArguments] identifying arguments by looking nouns governed by verbs")
			//for(sent <- doc.sentences; arg <- potentialArgsByHead(sent)) {
			//	newDoc.addArgument(arg)
			//}
			for((sentIdx, preds) <- newDoc.predicates.groupBy(_.location.getSentenceIdx))
				for(arg <- potentialArgsByDeps(newDoc.sentences(sentIdx), preds))
					newDoc.addArgument(arg)
		}
		if(verbose)
			println("[identifyPredicatesAndArguments] doc.#arguments=%d doc.#corefs=%d #deps=%d"
				.format(newDoc.arguments.size, newDoc.corefs.size, newDoc.sentences.map(_.dependencies.size).sum))
		
		newDoc
	}
	
	/**
	 * finds the `nounCutoff` most common nouns and `verbCutoff` most common
	 * verbs and counts them as stopwords
	 */
	def stopwordsBasedOnFrequency(docs: Seq[Document], nounCutoff: Int = 30, verbCutoff: Int = 30): Set[String] = {
		val nounCounts = new HashMap[String, Int]
		val verbCounts = new HashMap[String, Int]
		docs.flatMap(_.sentences).flatMap(_.tokens).foreach(t => {
			val key = t.getWord.toLowerCase
			if(t.getPosTag.startsWith("N")) {
				val c = nounCounts.getOrElse(key, 0)
				nounCounts.update(key, c+1)
			}
			if(t.getPosTag.startsWith("V")) {
				val c = verbCounts.getOrElse(key, 0)
				verbCounts.put(key, c+1)
			}
		})
		val stopNouns = nounCounts.keys.toIndexedSeq.sortBy(n => nounCounts(n)).takeRight(nounCutoff)
		val stopVerbs = verbCounts.keys.toIndexedSeq.sortBy(n => verbCounts(n)).takeRight(verbCutoff)	 
		//println("[PredArgSelector stopwords] nounCutoff=%d, stopNouns: [%s]".format(nounCutoff, stopNouns.reverse.mkString(", ")))
		//println("[PredArgSelector stopwords] verbCutoff=%d, stopVerbs: [%s]".format(nounCutoff, stopVerbs.reverse.mkString(", ")))
		(stopNouns ++ stopVerbs).toSet
	}
	
	/**
	 * set returned is all lower case
	 */
	val stopwordsAPriori = {
		val rv = new ReportingVerbs
		rv.setup(Seq())
		val did = Set("do", "did", "didn")
		val is = Set("is", "are", "be", "become", "became", "am", "was", "were", "been", "being")
		val have = Set("have", "had", "has", "hadn", "got", "get", "getting", "gets")
		val other = Set("give", "gave", "made", "take", "took", "%")
		val stopwords = (did | is | have | other | rv.getReportingVerbs).map(_.toLowerCase)
		//println("[PredArgSelector stopwords] %s".format(stopwords.mkString(", ")))
		stopwords
	}
	
}

