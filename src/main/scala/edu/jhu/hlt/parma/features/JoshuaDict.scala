// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder
import edu.jhu.hlt.parma.util.Describe
import scala.collection.JavaConversions._
import redis.clients.jedis._


object QueryLanguage extends Enumeration {
	type QueryLanguage = Value
	val English, Spanish = Value
}

// client
class JoshuaDict extends AlignmentSimilarity {
	type JoshuaMap = Map[String, Seq[JoshuaRule]]
	
	override def name = "JoshuaDict"

	@transient
	lazy val jedis = {
		val redis = new Jedis(RedisStuff.hostname, RedisStuff.port)
		if(redis.ping != "PONG")
			throw new RuntimeException("cannot connect to redis! " + RedisStuff.toString)
		redis
	}

	@transient
	lazy val joshuaKeys = RedisBloomFilter.initialize(true)

	def translate(word: String, ql: QueryLanguage.Value): Array[String] = {
		val w = word.toLowerCase
		pipeRedis(Seq(w), ql)(w).map(_.rhs).toSet.toArray
	}

	def translate(word: String): Array[String] = {
		translate(word, QueryLanguage.English)
	}

	def translate(words: Array[String], ql: QueryLanguage.Value): Array[Array[String]] = {
		val ws = words.map(_.toLowerCase)
		val joshuamap = pipeRedis(ws, ql)
		ws.map{ word =>
			joshuamap(word).map(_.rhs).toSet.toArray
		}.toArray
	}

	def translate(words: Array[String]): Array[Array[String]] = {
		translate(words, QueryLanguage.English)
	}

	def pipeRedis(keys: Iterable[String], ql: QueryLanguage.Value): JoshuaMap = {
		val dbNumber = ql match {
			case QueryLanguage.English => 2
			case QueryLanguage.Spanish => 3
		}
    jedis.select(dbNumber)
		val pipe = jedis.pipelined
		keys.foreach {w => if (joshuaKeys.in(w)) pipe.lrange(w, 0, 5)}
		val results = pipe.syncAndReturnAll.map{ res =>
			res.asInstanceOf[java.util.List[String]].map{ rule => str2rule(rule) }
		}.map{ entry => if (entry.isEmpty) None
										else Some((entry.head.lhs, entry))}.flatten
		results.toMap.withDefaultValue(Seq())
	}

	def str2rule(redisStr: String): JoshuaRule = {
		val ar = redisStr.split("\t")
		val lhs = ar(0)
		val rhs = ar(1)
		val score = ar(2).toDouble
		new JoshuaRule(lhs, rhs, score)
	}

	def possibleAlignments(a: String, b: Seq[String], joshuaMap: JoshuaMap): Seq[JoshuaRule] = {
		joshuaMap(a).filter(rule => b.contains(rule.rhs))
		//queryRedis(a).filter(rule => b.contains(rule.rhs))
	}

	def alignWordToSpan(a: String, b: Seq[String], joshuaMap: JoshuaMap): Option[JoshuaRule] = {
		lazy val pas = possibleAlignments(a,b, joshuaMap)
		if (pas.isEmpty) None
		else Some(pas.maxBy(_.score))
	}

	def alignSpanToSpan(as: Seq[String], bs: Seq[String], joshuaMap: JoshuaMap): (Seq[JoshuaRule], Int) = {
		val rules = as.flatMap{a => alignWordToSpan(a, bs, joshuaMap)}
		//val alignment = as.zip(rules)
		//val wordScores = alignment.collect{case (a,rule) => (a, rule.rhs, rule.score)}
		val unaligned = as.length - rules.length
		(rules, unaligned)
	}

	def scoreAlignment(alignment: Seq[JoshuaRule], unaligned: Int, penalty: Int): Double = {
		alignment.view.map(_.score).sum + (penalty*unaligned)
	}

	def align(reportSpan: Seq[String], passageSpan: Seq[String], english: JoshuaMap, 
		spanish: JoshuaMap): ((Seq[JoshuaRule],Int),(Seq[JoshuaRule], Int)) = {
		val forward = alignSpanToSpan(reportSpan, passageSpan, english)
		val backward = alignSpanToSpan(passageSpan, reportSpan, spanish)
		(forward, backward)
	}

	def termSimilarity(reportSpan: Seq[String], passageSpan: Seq[String], penalty: Int, 
		english: JoshuaMap, spanish: JoshuaMap): Double = {
		val ((frules,funaligned),(brules,bunaligned)) = align(reportSpan,passageSpan, english, spanish)
		if (frules.isEmpty || brules.isEmpty) -100.0
		else {
			//term scores = span score / aligned terms
			val fScore = scoreAlignment(frules, funaligned, penalty) / (reportSpan.size - funaligned)
			val bScore = scoreAlignment(brules, bunaligned, penalty) / (passageSpan.size - bunaligned) 
			//term similarity = average of forward and backward alignment score
			(fScore + bScore) / -2.0
		}
	}

	

	private[this] val binarizer = new edu.jhu.hlt.parma.util.FixedWidthBinarizer(5, false, -64d, -8d)
	private[this] val debug = false


	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {
		//report is English, passage is Spanish

		val penalty = 10 //parameter penalizing unaligned words, should maybe be tuned
		
		val (rcm, pcm) = CanonicalMentionFinder.canonicalMentions(a, report, passage)
		
		val rs = report.getSentence(rcm).toString.toLowerCase.split(" ")	// report.getMentionTokens(rcm).map(_.getWord)
		val ps = passage.getSentence(pcm).toString.toLowerCase.split(" ")

		lazy val englishMap = pipeRedis(rs, QueryLanguage.English)
		lazy val spanishMap = pipeRedis(ps, QueryLanguage.Spanish)


		//for(cutoff <- List(5d, 12d, 25d)) {
		for(cutoff <- 2d to 7d by 0.2d map { x => x*x }) {

			val morePureEnglishMap = englishMap.mapValues(_.filter(_.score <= cutoff)).withDefaultValue(Seq[JoshuaRule]())
			val morePureSpanishMap = spanishMap.mapValues(_.filter(_.score <= cutoff)).withDefaultValue(Seq[JoshuaRule]())

			val sentenceScore = termSimilarity(rs, ps, penalty, morePureEnglishMap, morePureSpanishMap)

			val rspan = report.getMentionString(rcm).split(" ")
			val pspan = passage.getMentionString(pcm).split(" ")
			val mentionScore = termSimilarity(rspan, pspan, penalty, morePureEnglishMap, morePureSpanishMap)

			val rhead = Seq(report.getHeadString(rcm))
			val phead = Seq(passage.getHeadString(pcm))
			val headScore = termSimilarity(rhead, phead, penalty, morePureEnglishMap, morePureSpanishMap)

			if(debug) {
				println("report mention = " + Describe.mentionInContext(rcm, report))
				println("passage mention = " + Describe.mentionInContext(pcm, passage))
				println("sentence score = " + sentenceScore)
				println("mention score = " + mentionScore)
				println("head score = " + headScore)
			}

			featureIndexer.start(sv)
			featureIndexer.addStable("sts-sentence", sentenceScore, binarizer)
			featureIndexer.addStable("sts-mention", mentionScore, binarizer)
			featureIndexer.addStable("sts-head", headScore, binarizer)
			featureIndexer.commit
		}
	}

}


/* vim: set noet : */
