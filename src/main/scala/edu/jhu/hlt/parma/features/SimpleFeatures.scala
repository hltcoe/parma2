
package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import Math.{ min, max }
import collection.JavaConversions._
import collection.mutable.{ HashMap, HashSet, ArrayBuffer }
import java.io._
import java.util.zip._
import edu.mit.jwi._
import edu.mit.jwi.item._
import edu.mit.jwi.data.ILoadPolicy
import edu.mit.jwi.morph.WordnetStemmer

class SimpleFeatures extends AlignmentSimilarity {

	private var dict: IRAMDictionary = null
	private var stemmer: WordnetStemmer = null
	private var targetWord2Frame: collection.Map[String, Set[String]] = null
	private var argWord2FrameRole: collection.Map[String, Set[(String, String)]] = null	// values are (frame, role)

	// TODO some features use an int in their name (usually a max of two set size)
	// i don't think that we have enough training data (i saw one with 7 in the name
	// come up in the top 20 features once), and unless these features are coarsened
	// (say to ==0, >0, >1, >2, >3, >5, >8, >10) they will lead to overfitting.

	val useFramenet = true
	val lots = true	// if false, suppresses some features

	private def ser {
		val t0 = System.currentTimeMillis
		val ft = ParmaConfig.getFile("features.framenet.new.targetWordMap.cache")
		log("[SimpleFeatures ser] writing to " + ft.getPath)
		var dos = new DataOutputStream(new GZIPOutputStream(new FileOutputStream(ft)))
		dos.writeInt(targetWord2Frame.size)
		for((k,v) <- targetWord2Frame) {
			dos.writeUTF(k)
			dos.writeInt(v.size)
			v.foreach(dos.writeUTF)
		}
		dos.close

		val t1 = System.currentTimeMillis
		val fa = ParmaConfig.getFile("features.framenet.new.argWordMap.cache")
		log("[SimpleFeatures ser] writing to " + fa.getPath)
		dos = new DataOutputStream(new GZIPOutputStream(new FileOutputStream(fa)))
		dos.writeInt(argWord2FrameRole.size)
		for((k,v) <- argWord2FrameRole) {
			dos.writeUTF(k)
			dos.writeInt(v.size)
			v.foreach(pair => {
				dos.writeUTF(pair._1)
				dos.writeUTF(pair._2)
			})
		}
		dos.close
		val t2 = System.currentTimeMillis
		log("[SimpleFeatures ser] targets took %.1f seconds, args took %.1f".format((t1-t0)/1000d, (t2-t1)/1000d))
	}

	private def deser {
		val t0 = System.currentTimeMillis
		targetWord2Frame = new HashMap[String, Set[String]]
		val ft = ParmaConfig.getFile("features.framenet.new.targetWordMap.cache")
		log("[SimpleFeatures deser] reading from " + ft.getPath)
		var dis = new DataInputStream(new GZIPInputStream(new FileInputStream(ft)))
		val numTargets = dis.readInt
		var i = 0
		while(i < numTargets) {
			i += 1
			val k = dis.readUTF
			val n = dis.readInt
			val buf = new ArrayBuffer[String]
			for(i <- 0 until n) buf += dis.readUTF
			targetWord2Frame += ((k, buf.toSet))
		}
		dis.close
		
		val t1 = System.currentTimeMillis
		argWord2FrameRole = new HashMap[String, Set[(String, String)]]
		val fa = ParmaConfig.getFile("features.framenet.new.argWordMap.cache")
		log("[SimpleFeatures deser] reading from " + fa.getPath)
		dis = new DataInputStream(new GZIPInputStream(new FileInputStream(fa)))
		val numArgs = dis.readInt
		i = 0
		while(i < numArgs) {
			i += 1
			val k = dis.readUTF
			val n = dis.readInt
			val buf = new ArrayBuffer[(String, String)]
			for(i <- 0 until n) {
				val kk = dis.readUTF
				val vv = dis.readUTF
				buf += ((kk, vv))
			}
			argWord2FrameRole += ((k, buf.toSet))
		}
		dis.close
		val t2 = System.currentTimeMillis
		log("[SimpleFeatures deser] targets took %.1f seconds, args took %.1f".format((t1-t0)/1000d, (t2-t1)/1000d))
	}

	private def computeFN {
		val ft = ParmaConfig.getFile("features.framenet.new.targetWordMap.cache")
		val fa = ParmaConfig.getFile("features.framenet.new.targetWordMap.cache")
		assert(ft.isFile == fa.isFile)
		if(ft.isFile)
			deser
		else {
			import edu.jhu.hlt.fnparse.data._
			import edu.jhu.hlt.fnparse.datatypes._
			val t = System.currentTimeMillis
			targetWord2Frame = new HashMap[String, Set[String]]
			argWord2FrameRole = new HashMap[String, Set[(String, String)]]
			val cf = ParmaConfig.getFile("features.framenet.lexConll")
			val ff = ParmaConfig.getFile("features.framenet.lexFrames")
			var i = 0
			val start = System.currentTimeMillis
			for(fi <- new FileFrameInstanceProvider(ff, cf).getParsedOrTaggedSentences.flatMap(_.getFrameInstances)) {
				val s = fi.getSentence
				val t = fi.getTarget
				val fr = fi.getFrame
				if(t.width == 1) {
					val tw = s.getWord(t.start).toLowerCase.intern
					val sf = targetWord2Frame.getOrElse(tw, Set[String]())
					targetWord2Frame += ((tw, sf + fr.getName))
				}

				for(i <- 0 until fr.numRoles) {
					val a = fi.getArgument(i)
					if(a.width == 1) {
						val aw = s.getWord(a.start).toLowerCase.intern
						val sf = argWord2FrameRole.getOrElse(aw, Set[(String, String)]())
						argWord2FrameRole += ((aw, sf + ((fr.getName, fr.getRole(i)))))
					}
				}

				if(i % 2500 == 0) {
					val elaps = (System.currentTimeMillis - start) / 1000d
					val rate = i / elaps
					val rem = (151135 - i) / rate
					log("[SimpleFeatures] loading framenet lex examples %d %.1f sec in, rate %.1f, %.1f (est) remaining"
						.format(i, elaps, rate, rem))
				}
				i += 1
			}
			log("[SimpleFeatures] load framenet in %.1f seconds".format((System.currentTimeMillis - t) / 1000d))

			ser
		}
	}

	override def setup(calibrateOn: java.util.Collection[DocAlignment]) {
		// load wordnet
		val t = System.currentTimeMillis
		dict = new RAMDictionary(ParmaConfig.getFile("features.wordnet.datapath"), ILoadPolicy.IMMEDIATE_LOAD)
		dict.open
		stemmer = new WordnetStemmer(dict)
		log("[SimpleFeatures] load wordnet in %.1f seconds".format((System.currentTimeMillis - t) / 1000d))

		// load framenet
		if(useFramenet)
			computeFN
	}


	def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {
		val (reportMention, passageMention) = CanonicalMentionFinder.canonicalMentions(a, report, passage)
		val rSent: Sentence = report.getSentence(reportMention)
		val pSent: Sentence = passage.getSentence(passageMention)
		val rh: Token = report.getHeadToken(reportMention)
		val ph: Token = passage.getHeadToken(passageMention)


		// intercept
		b(sv, "intercept")


		// wordnet
		(tok2wnWord(rh), tok2wnWord(ph)) match {
			case (Some(rw), Some(pw)) => {

				// same synset
				if(rw.getSynset.getWords.contains(pw)) {
					b(sv, "same-synset")
					b(sv, "same-synset", rh.getPosTag)
				}

				// related synsets
				import collection.JavaConversions._
				for((relation, ssids) <- rw.getSynset.getRelatedMap; ssid <- ssids) {
					if(dict.getSynset(ssid).getWords.contains(pw)) {
						b(sv, "related-synset")
						b(sv, "related-synset", rh.getPosTag)
						b(sv, "related-synset", ph.getPosTag)
						b(sv, "related-synset", relation.getName)
						if(lots)
							b(sv, "related-synset", relation.getName, rh.getPosTag)
					}
				}
			}
			case _ => {}
		}


		// framenet
		if(useFramenet) {
			val emptyStrSet = Set[String]()

			// target examples from LEX
			val rTargets = targetWord2Frame.getOrElse(rh.getWord.toLowerCase, emptyStrSet)
			val pTargets = targetWord2Frame.getOrElse(ph.getWord.toLowerCase, emptyStrSet)
			val commonTargets = rTargets & pTargets
			if(commonTargets.isEmpty) {
				b(sv, "no-common-frame/targets")
				b(sv, "no-common-frame/targets", max(rTargets.size, pTargets.size).toString)
				b(sv, "no-common-frame/targets", rh.getPosTag)
				b(sv, "no-common-frame/targets", ph.getPosTag)
				if(lots) {
					b(sv, "no-common-frame/targets", max(rTargets.size, pTargets.size).toString, rh.getPosTag)
					b(sv, "no-common-frame/targets", max(rTargets.size, pTargets.size).toString, ph.getPosTag)
				}
			}
			else {
				b(sv, "common-frame/target")
				for(i <- Seq(1, 2, 3, 5, 8, 15)) {
					if(commonTargets.size > i)
						b(sv, "common-frame/target->"+i)
				}
				b(sv, "common-frame/target", rh.getPosTag)
				b(sv, "common-frame/target", ph.getPosTag)
				if(lots) {
					b(sv, "common-frame/target", commonTargets.size.toString, rh.getPosTag)
					b(sv, "common-frame/target", commonTargets.size.toString, ph.getPosTag)
					for(ft <- commonTargets)
						b(sv, "common-frame/target", ft)
				}
			}

			// arg examples from LEX
			val emtyStrPSet = Set[(String, String)]()
			val rFR = argWord2FrameRole.getOrElse(rh.getWord.toLowerCase, emtyStrPSet)
			val pFR = argWord2FrameRole.getOrElse(ph.getWord.toLowerCase, emtyStrPSet)
			val commonFrameRoles = rFR.toSet & pFR.toSet
			if(commonFrameRoles.isEmpty) {
				b(sv, "no-common-frameRoles")
				b(sv, "no-common-frameRoles", rh.getPosTag)
				b(sv, "no-common-frameRoles", ph.getPosTag)
				if(lots) {
					b(sv, "no-common-frameRoles", max(rFR.size, pFR.size).toString)
					b(sv, "no-common-frameRoles", max(rFR.size, pFR.size).toString, rh.getPosTag)
					b(sv, "no-common-frameRoles", max(rFR.size, pFR.size).toString, ph.getPosTag)
				}
			}
			else {
				b(sv, "common-frameRole")
				b(sv, "common-frameRole", rh.getPosTag)
				b(sv, "common-frameRole", ph.getPosTag)
				if(lots) {
					b(sv, "common-frameRole", commonFrameRoles.size.toString)
					b(sv, "common-frameRole", commonFrameRoles.size.toString, rh.getPosTag)
					b(sv, "common-frameRole", commonFrameRoles.size.toString, ph.getPosTag)
					for(fr <- commonFrameRoles) {
						b(sv, "common-frameRole", fr._1, fr._2)
						b(sv, "common-frameRole", fr._1)
					}
				}
			}
		}




		// word match
		b(sv, rh.getPosTag, ph.getPosTag)
		b(sv, 0.2d, rh.getLemma, ph.getLemma)
		if(rh.getWord == ph.getWord) {
			b(sv, "exact-word-match")
			b(sv, "exact-word-match", rh.getPosTag)
		}
		if(rh.getWord.toLowerCase == ph.getWord.toLowerCase) {
			b(sv, "nocase-word-match")
			b(sv, "nocase-word-match", rh.getPosTag)
		}
		if(rh.getLemma == ph.getLemma) {
			b(sv, "lemma-match")
			b(sv, "lemma-match", rh.getPosTag)
		}
		val s1 = rh.getWord.toLowerCase
		val s2 = ph.getWord.toLowerCase
		for(((c1,c2), len) <- s1.zip(s2).takeWhile(cc => cc._1 == cc._2).zipWithIndex) {
			val w = math.min(2d, math.pow(1.5d, len) / 7d)
			b(sv, w, "prefix-match-" + len)
			if(lots)
				b(sv, w, "prefix-match-" + len, rh.getPosTag)
		}


		// dependents
		val rDeps: Seq[Dependency[Token]] = rSent.governedBy(reportMention)
		val pDeps: Seq[Dependency[Token]] = pSent.governedBy(passageMention)

		// i'm using lemma for now
		def depWord(d: Dependency[Token]) = d.map(t => "erased", _.getLemma)
		val tDepWordInt = rDeps.map(depWord).toSet & pDeps.map(depWord).toSet
		tDepWordInt.headOption match {
			case Some(td) =>
				b(sv, "typed-dep-word-overlap")
				if(lots)
					b(sv, "typed-dep-word-overlap", td.typ)
			case None => {}
		}
		val utDepWordInt = rDeps.map(_.toUntyped).map(depWord).toSet &
			pDeps.map(_.toUntyped).map(depWord).toSet
		utDepWordInt.headOption match {
			case Some(td) =>
				b(sv, "untyped-dep-word-overlap")
				if(lots)
					b(sv, "untyped-dep-word-overlap", td.typ)
			case None => {}
		}

		def depPos(d: Dependency[Token]) = d.map(t => "erased", _.getPosTag)
		val tDepPosInt = rDeps.map(depPos).toSet & pDeps.map(depPos).toSet
		tDepPosInt.headOption match {
			case Some(td) =>
				b(sv, "typed-dep-pos-overlap")
				b(sv, "typed-dep-pos-overlap", td.typ)
				if(lots) {
					b(sv, "typed-dep-pos-overlap", td.dep)
					b(sv, "typed-dep-pos-overlap", td.typ, td.dep)
				}
			case None => {}
		}
		val utDepPosInt = rDeps.map(_.toUntyped).map(depPos).toSet &
			pDeps.map(_.toUntyped).map(depPos).toSet
		utDepPosInt.headOption match {
			case Some(td) =>
				b(sv, "untyped-dep-pos-overlap")
				b(sv, "untyped-dep-pos-overlap", td.typ)
				if(lots) {
					b(sv, "untyped-dep-pos-overlap", td.dep)
					b(sv, "untyped-dep-pos-overlap", td.typ, td.dep)
				}
			case None => {}
		}


		// governors
		val rGovs: Seq[Dependency[Token]] = rSent.governs(MentionBuilder.from(rSent, rh))
		val pGovs: Seq[Dependency[Token]] = pSent.governs(MentionBuilder.from(pSent, ph))

		def govWord(d: Dependency[Token]) = d.map(_.getLemma, t => "erased")
		val tGovWordInt = rGovs.map(govWord).toSet & pGovs.map(govWord).toSet
		tGovWordInt.headOption match {
			case Some(td) =>
				b(sv, "typed-gov-word-overlap")
				b(sv, "typed-gov-word-overlap", td.typ)
			case None => {}
		}
		val utGovWordInt = rGovs.map(_.toUntyped).map(govWord).toSet &
			pGovs.map(_.toUntyped).map(govWord).toSet
		utGovWordInt.headOption match {
			case Some(td) =>
				b(sv, "untyped-gov-word-overlap")
				b(sv, "untyped-gov-word-overlap", td.typ)
			case None => {}
		}

		def govPos(d: Dependency[Token]) = d.map(_.getPosTag, t => "erased")
		val tGovPosInt = rGovs.map(govPos).toSet & pGovs.map(govPos).toSet
		tGovPosInt.headOption match {
			case Some(td) =>
				b(sv, "typed-gov-pos-overlap")
				b(sv, "typed-gov-pos-overlap", td.typ)
				if(lots) {
					b(sv, "typed-gov-pos-overlap", td.gov)
					b(sv, "typed-gov-pos-overlap", td.typ, td.gov)
				}
			case None => {}
		}
		val utGovPosInt = rGovs.map(_.toUntyped).map(govPos).toSet &
			pGovs.map(_.toUntyped).map(govPos).toSet
		utGovPosInt.headOption match {
			case Some(td) =>
				b(sv, "untyped-gov-pos-overlap")
				b(sv, "untyped-gov-pos-overlap", td.typ)
				if(lots) {
					b(sv, "untyped-gov-pos-overlap", td.gov)
					b(sv, "untyped-gov-pos-overlap", td.typ, td.gov)
				}
			case None => {}
		}


		// pos sequence pairs
		if(lots) {
			val rPosSeq = rSent(reportMention).map(_.getPosTag).mkString("-")
			val pPosSeq = pSent(passageMention).map(_.getPosTag).mkString("-")
			b(sv, 0.1d, rPosSeq, pPosSeq)
			b(sv, 0.1d, pPosSeq, rPosSeq)
		}
	}

	private def tok2wnWord(t: Token): Option[IWord] = {
		val pos = ptb2wordNet(t.getPosTag)
		if(pos == null) return None
		val stems = stemmer.findStems(t.getWord, pos)
		if(stems == null || stems.size == 0) return None
		val idxWord = dict.getIndexWord(stems.get(0), pos)
		if(idxWord == null) return None
		val iwordId = idxWord.getWordIDs.get(0)
		Some(dict.getWord(iwordId))
	}

	def ptb2wordNet(ptbPosTag: String): edu.mit.jwi.item.POS = {
		if(ptbPosTag.startsWith("J"))
			return edu.mit.jwi.item.POS.ADJECTIVE;
		if(ptbPosTag.startsWith("R"))
			return edu.mit.jwi.item.POS.ADVERB;
		if(ptbPosTag.startsWith("N"))
			return edu.mit.jwi.item.POS.NOUN;
		if(ptbPosTag.startsWith("V"))
			return edu.mit.jwi.item.POS.VERB;
		//if(pedantic)
		//	throw new IllegalArgumentException("is this a ptb tag? " + ptbPosTag);
		return null;
	}

}


