package edu.jhu.hlt.parma.inference.temporal

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import collection.mutable.ArrayBuffer
import collection.JavaConversions._
import java.io._

trait TemporalRelationPredictor extends TemporalOrderPredictor {

	def relationship(p1: Predicate, p2: Predicate): Option[TimeMLRelation]

	// inherited from TemporalOrderPredictor
	override def after(p1: Predicate, p2: Predicate): Double = {
		relationship(p1, p2) match {
			case None => 0d
			case Some(Before(_, _, conf)) => -conf
			case Some(After(_, _, conf)) => conf
			case Some(ImmediatelyBefore(_, _, conf)) => -conf
			case Some(ImmediatelyAfter(_, _, conf)) => conf
			case Some(Begins(_, _, conf)) => -conf/2d
			case Some(BegunBy(_, _, conf)) => conf/2d
			case Some(Ends(_, _, conf)) => conf/2d
			case Some(EndedBy(_, _, conf)) => -conf/2d
			case Some(Includes(_, _, _)) => 0d
			case Some(IsIncluded(_, _, _)) => 0d
			case Some(Simultaneous(_, _, _)) => 0d
			case _ => 0d	// i can't tell how i'm not covering all the cases...
		}
	}
}

/**
 * asks TimeSieve for the ordering over predicates in this document
 * (limits to the predicates passed in)
 */
class TimeSieveTLinks(val doc: Document, val preds: IndexedSeq[Predicate])
		extends TemporalRelationPredictor with Logging2 {

	import timesieve._
	import timesieve.tlink._

	def this(doc: Document) = this(doc, doc.predicates)

	lazy val tlinks: Map[(Predicate, Predicate), TimeMLRelation] = {
		val useCache = ParmaConfig.getBoolean("features.timesieve.tlink-cache", false)
		lazy val f = cacheLocation
		if(useCache && f.exists) {
			log("[TimeSieveTLinks] loading tlinks for %s from %s".format(doc.id, f.getPath))
			val ois = new ObjectInputStream(new FileInputStream(f))
			val m = ois.readObject.asInstanceOf[Map[(Predicate, Predicate), TimeMLRelation]]
			ois.close
			m
		} else {
			val m = tlinksNoCache
			if(useCache) {
				log("[TimeSieveTLinks] serializing tlinks for %s to %s".format(doc.id, f.getPath))
				val oos = new ObjectOutputStream(new FileOutputStream(f))
				oos.writeObject(m)
				oos.close
			}
			m
		}
	}

	private def tlinksNoCache: Map[(Predicate, Predicate), TimeMLRelation] = {

		TimeSieveUtil.loadWordnet

		if(System.getenv("JWNL") == null) {
			warn("you need to specify a wordnet properties file using the JWNL environment variable key")
			warn("try running `source scripts/setup-wordnet-for-timesieve.sh`")
		}


		// stores mapping between time-sieve "event instane ids" (eiid's) and Predicates
		val eiid2pred = new collection.mutable.HashMap[String, Predicate]

		// setup the SieveDocument
		val sieveDoc = new SieveDocument(doc.id)
		for(s <- doc.sentences) {
			val text = s.tokens.map(_.getWord).mkString(" ")
			val parse = TreeUtils.sExpressionForParse(s.ptbParse.get)
			log("parse = " + parse)
			def tok2str(t: Token): String = {
				"%s-%d".format(
					t.getWord.replaceAll(" ", "\u00A0").replaceAll("-", "_"),
					t.index + 1)
			}
			val deps = s.dependencies.map(_.toTimeSieveStr(tok2str)).mkString("\n")
			log("deps = " + deps)
			val events = new java.util.ArrayList[TextEvent]
			val timexes = new java.util.ArrayList[Timex]
			sieveDoc.addSentence(text, parse, deps, events, timexes)
		}
		for((p, idx) <- preds.zipWithIndex) {
			val sentIdx: Int = p.location.getSentenceIdx
			val index = p.location.getHeadTokenIdx + 1	// stanford indices start at 1
			val e = new TextEvent(doc.getMentionString(p), "TextEvent"+index, sentIdx, index)
			val eiid = "e"+index
			eiid2pred += (eiid -> p)
			e.addEiid(eiid)
			val events = new java.util.ArrayList[TextEvent]
			events.add(e)
			sieveDoc.addEvents(sentIdx, events)
		}
		log("[TemporalOrderPredictor] made a SieveDocument with %d events".format(sieveDoc.getEvents.size))

		// setup the classifier
		val classifier = {
			val tLinkModelDir = ParmaConfig.getDirectory("features.timesieve.tlink-model-dir")
			val props = new java.util.Properties
			props.setProperty("prob", "0.001")
			props.setProperty("eesame", "1")
			props.setProperty("etsame", "1")
			props.setProperty("eediff", "1")
			props.setProperty("edct", "1")
			val sdocs = new SieveDocuments
			sdocs.addDocument(sieveDoc)
			new TLinkClassifier(sdocs, tLinkModelDir.getPath, props)
		}

		classifier.extractTLinks()
		val links = sieveDoc.getTlinks
		log("[TemporalOrderPredictor] found %d tlinks in document %s (%d predicates, %d in same sent, %d adjacent)"
			.format(links.size, doc.id, preds.size, numPairsInSameSent, numPairsInAdjSent))
		val m = new ArrayBuffer[((Predicate, Predicate), TimeMLRelation)]
		for(l <- links) {
			val p1 = eiid2pred(l.getId1)
			val p2 = eiid2pred(l.getId2)
			val r = convertRelation(l, p1, p2)
			m += (((p1, p2), r))
		}
		m.toMap
	}

	def convertRelation(tlink: TLink, p1: Predicate, p2: Predicate): TimeMLRelation = tlink.getRelation match {
		case TLink.Type.AFTER => After(p1, p2, tlink.getRelationConfidence)
		case TLink.Type.BEFORE => Before(p1, p2, tlink.getRelationConfidence)
		case TLink.Type.BEGUN_BY => BegunBy(p1, p2, tlink.getRelationConfidence)
		case TLink.Type.BEGINS => Begins(p1, p2, tlink.getRelationConfidence)
		case TLink.Type.ENDED_BY => EndedBy(p1, p2, tlink.getRelationConfidence)
		case TLink.Type.ENDS => Ends(p1, p2, tlink.getRelationConfidence)
		case TLink.Type.INCLUDES => Includes(p1, p2, tlink.getRelationConfidence)
		case TLink.Type.IS_INCLUDED => IsIncluded(p1, p2, tlink.getRelationConfidence)
		case TLink.Type.IBEFORE => ImmediatelyBefore(p1, p2, tlink.getRelationConfidence)
		case TLink.Type.IAFTER => ImmediatelyAfter(p1, p2, tlink.getRelationConfidence)
		case TLink.Type.SIMULTANEOUS => Simultaneous(p1, p2, tlink.getRelationConfidence)
		case _ => throw new RuntimeException
	}

	def numPairsInSameSent: Int = {
		var c = 0
		for(p1 <- preds; p2 <- preds if p1 != p2) {
			if(p1.location.getSentenceIdx == p2.location.getSentenceIdx)
				c += 1
		}
		c
	}

	def numPairsInAdjSent: Int = {
		var c = 0
		for(p1 <- preds; p2 <- preds if p1 != p2) {
			val d = p1.location.getSentenceIdx - p2.location.getSentenceIdx
			if(d * d <= 1) c += 1
		}
		c
	}

	override def relationship(p1: Predicate, p2: Predicate): Option[TimeMLRelation] = {
		val l = tlinks.get((p1,p2))
		if(l.isEmpty) tlinks.get((p2,p1))
		else l
	}

	def cacheLocation: File = {
		val hc = doc.hashCode ^ preds.hashCode
		val parent = ParmaConfig.getDirectory("features.timesieve.tlink-cache-loc")
		new File(parent, "%s-%s.tlinks".format(doc.id, java.lang.Integer.toHexString(hc)))
	}
}

object TimeSieveUtil {
	// there is a slight bug where TimeSieve forgets to load WordNet
	// this loads it manually
	lazy val m = new timesieve.Main
	def loadWordnet {
		require(m != null)
	}
}

/**
 * for testing, just prints stuff
 */
object TimeSieveTLinks extends Logging2 {

	import edu.jhu.hlt.parma.input._
	import java.io.File
	//redirectLogTo(new File("log.txt"))
	//teeLogTo(StdOutLogger, new FileLogger("log.txt"))

	def main(args: Array[String]) {

		ParmaConfig.load("parma.config")
		val docs = ConcreteDocAlignmentReader.EECB.daIter.take(25).flatMap(da => Seq(da.report, da.passage)).toSet[Document].toIndexedSeq
		val tss = docs.map(d => new TimeSieveTLinks(d, d.predicates))
		tss.foreach(_.redirectLogTo(this))
		log("\ntotal links = %d\n\n".format(tss.map(_.tlinks.size).sum))
		log("%d documents and %d predicates\n".format(docs.size, docs.map(_.predicates.size).sum))
		var hasR = 0
		var totalR = 0
		for((d, ts) <- docs.zip(tss) if d.predicates.size > 1) {
			log("%s has %d predicates".format(d.id, d.predicates.size))
			for(p1 <- d.predicates; p2 <- d.predicates if p1 != p2) {
				totalR += 1
				ts.relationship(p1, p2) match {
					case Some(r) =>
						hasR += 1
						log("%s".format(r))
					case None =>
						log("no relation for %s and %s".format(Describe.predicate(p1, d), Describe.predicate(p2, d)))
				}
			}
		}
		log("%d of %d pairs of predicates have a TimeML relation".format(hasR, totalR))

	}

}


