package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder
import edu.jhu.hlt.parma.feature_interfaces._

object WiktionaryFeature {
	def main(args: Array[String]) {
		if(args.length != 1) {
			println("please give a config file")
			return
		}
		ParmaConfig.load(args(0))
		val wf = new WiktionaryFeature
		wf.setup(null)
		import collection.JavaConversions._
		for((wordType, syns) <- wf.synonyms)
			println("%s => %s".format(wordType, syns.mkString(", ")))

		// make sure the POS tags match up
		import edu.jhu.hlt.parma.input._
		val das = ConcreteDocAlignmentReader.EECB.getDocAlignments
		val docPos = das
			.flatMap(da => Seq(da.report, da.passage))
			.flatMap(_.allTokens)
			.map(_.getPosTag)
			.toSet[String]
		val wiktPos = wf.synonyms.keySet.map(_.pos).toSet[String]
		println("from docs: " + docPos.mkString(", "))
		println("from wikt: " + wiktPos.mkString(", "))
		// from docs: IN, ``, AUXN, RP, AUXD, EX, PRP, WRB, ., WDT, AUXG, WP, VBP, PDT, NN, JJR, -LRB-, MD, PRP$, VBG, JJ, CC, POS, ,, RBS, NNP, FW, JJS, AUXB, -RRB-, AUXZ, NNS, VBZ, DT, VB, WP$, CD, RBR, UH, NNPS, RB, :, VBN, TO, $, '', SYM, VBD, AUXP
		// from wikt: NOUN, NUMERAL, AFFIX, PROVERB, ADVERB, INITIALISM, CONJUNCTION, ACRONYM, SUFFIX, PUNCTUATION_MARK, IDIOM, PHRASE, null, VERB, PRONOUN, SYMBOL, INTERJECTION, ADJECTIVE, ABBREVIATION, PREPOSITION, NUMBER, PROPER_NOUN, PARTICLE, CONTRACTION, DETERMINER, PREFIX

		for(unknownTag <- Seq("AUXN", "AUXP", "AUXD", "AUXG",  "AUXB", "AUXZ", "WDT", "$", "PDT"))
			println(unknownTag + " => " + das
				.flatMap(da => Seq(da.report, da.passage))
				.flatMap(_.allTokens)
				.filter(_.getPosTag == unknownTag)
				.toSet[Token]
				.map(t => "%s %s".format(t.getWord, t.getPosTag))
				.mkString(",\n"))

		import edu.jhu.hlt.parma.inference.DocMetaAligner
		var all = 0
		var good = 0
		var preds = 0
		var alignments = 0
		for(da <- das) {
			for(a <- DocMetaAligner.allPossibleAlignments(da.context)) {
				alignments += 1
				val sv = wf.statelessFeaturize(a, da.report, da.passage)
				if(sv.l1 > 0d) {
					val (reportMention, passageMention) = CanonicalMentionFinder.canonicalMentions(a, da.report, da.passage)
					val rh = da.report.getHeadToken(reportMention)
					val ph = da.passage.getHeadToken(passageMention)
					if(rh.getLemma != ph.getLemma) {
						if(da.possibleAlignments.contains(a))
							good += 1
						all += 1
						if(a.isInstanceOf[PredicateAlignment])
							preds += 1
						println(Describe.alignment(a, da.report, da.passage))
					}
				}
			}
		}
		println("good=%d all=%d preds=%d alignments=%d".format(good, all, preds, alignments))
		// useLemma=true, caseSentive=false, ignorePOS=false => good=3 all=32 preds=27 alignments=51773
		// useLemma=true, caseSentive=false, ignorePOS=true =>  good=3 all=37 preds=30 alignments=51773
		// this feature is garbage!
	}
}

class WiktionaryFeature extends AlignmentSimilarity {

		val ptb2wiktPOS = Map(
			"IN" -> "PREPOSITION",
			"``" -> "PUNCTUATION_MARK",
			"AUXN" -> "",	// only token i found was "been"
			"RP" -> "PARTICLE",
			"AUXD" -> "",
			"EX" -> "",		// Existential there
			"PRP" -> "PRONOUN",	// Personal pronoun
			"WRB" -> "ADVERB",	// Wh-adverb
			"." -> "PUNCTUATION_MARK",
			"WDT" -> "",		// which, whatever, that
			"AUXG" -> "",
			"WP" -> "PRONOUN",		// Wh-pronoun
			"VBP" -> "VERB",
			"PDT" -> "",		// such, both, half, all
			"NN" -> "NOUN",
			"JJR" -> "ADJECTIVE",
			"-LRB-" -> "PUNCTUATION_MARK",
			"MD" -> "VERB",	// modal
			"PRP$" -> "PRONOUN",
			"VBG" -> "VERB",
			"JJ" -> "ADJECTIVE",
			"CC" -> "CONJUNCTION",
			"POS" -> "PUNCTUATION_MARK",
			"RBS" -> "ADVERB",
			"NNP" -> "PROPER_NOUN",
			"FW" -> "",		// foreign word
			"JJS" -> "ADJECTIVE",
			"AUXB" -> "",
			"-RRB-" -> "PUNCTUATION_MARK",
			"AUXZ" -> "",
			"NNS" -> "NOUN",
			"VBZ" -> "VERB",
			"DT" -> "DETERMINER",
			"VB" -> "VERB",
			"WP$" -> "PRONOUN",
			"CD" -> "NUMERAL",
			"RBR" -> "ADVERB",
			"UH" -> "INTERJECTION",
			"NNPS" -> "PROPER_NOUN",
			"RB" -> "ADVERB",
			":" -> "PUNCTUATION_MARK",
			"VBN" -> "VERB",
			"TO" -> "PREPOSITION",
			"$" -> "SYMBOL",
			"''" -> "PUNCTUATION_MARK",
			"SYM" -> "SYMBOL",
			"VBD" -> "VERB",
			"AUXP" -> "")

	val ignorePOS = false
	val caseSensitive = false
	val useLemma = true

	sealed case class WordType(val word: String, val pos: String)

	val synonyms = new java.util.HashMap[WordType, collection.mutable.Set[String]]

	override def setup(calibrateOn: java.util.Collection[DocAlignment]) {
		import org.json.simple._
		val f = ParmaConfig.getFile("features.wiktionary.json")
		val s = io.Source.fromFile(f).mkString
        val a = JSONValue.parse(s).asInstanceOf[JSONArray]
        val l = a.iterator()
        while(l.hasNext) {
            val o = l.next.asInstanceOf[JSONObject]
            val word = o.get("word").asInstanceOf[String]
            val pos = if(ignorePOS) "IGNORE_POS" else o.get("pos").asInstanceOf[String]
			try {
				val syns = o.get("SYNONYM").asInstanceOf[String].split(" \\|\\| ").map(_.split(":", 2).last)
				val synset = new collection.mutable.HashSet[String]
				synset ++= (if(caseSensitive) syns else syns.map(_.toLowerCase))
				val key = WordType(word, pos)
				val existing = synonyms.get(key)
				if(existing == null)
					synonyms.put(key, synset)
				else
					existing ++= synset
			} catch {
				case npe: NullPointerException => {}	// no SYNONYM key
			}
        }
	}

	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {
		val (reportMention, passageMention) = CanonicalMentionFinder.canonicalMentions(a, report, passage)
		val rh = report.getHeadToken(reportMention)
		val ph = passage.getHeadToken(passageMention)

		implicit def b2d(b: Boolean): Double = if(b) 1d else 0d
		featureIndexer.start(sv)
		featureIndexer.addStable("rp_word", lookup(rh, ph.getLemma))
		featureIndexer.addStable("rp_phrase", lookup(rh, passage.getMentionString(passageMention)))
		featureIndexer.addStable("pr_word", lookup(ph, rh.getLemma))
		featureIndexer.addStable("pr_phrase", lookup(ph, report.getMentionString(reportMention)))
		featureIndexer.commit
	}

	private[this] val empty = Set[String]()
	def lookup(left: Token, right: String): Boolean = {
		implicit def tok2wt(tok: Token) = {
			val w = if(useLemma) tok.getLemma else tok.getWord
			val pos = if(ignorePOS) "IGNORE_POS" else ptb2wiktPOS(tok.getPosTag.toUpperCase)
			WordType(if(caseSensitive) w else w.toLowerCase, pos)
		}
		val syns = synonyms.get(tok2wt(left))
		syns != null && syns.contains(right)
	}

}

