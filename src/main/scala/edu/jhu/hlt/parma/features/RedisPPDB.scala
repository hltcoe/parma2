// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.feature_interfaces.AlignmentSimilarity
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder
import edu.jhu.hlt.parma.util.SHA1er

import redis.clients.jedis._

import java.security.MessageDigest
import collection.JavaConversions._
import collection.mutable.{ Buffer, ArrayBuffer, HashMap }
import io.Source
import java.io._
import java.util.zip.GZIPInputStream

// client
class RedisPPDB extends AlignmentSimilarity {

	import edu.jhu.hlt.parma.features.RedisBloomFilter

	val useRedis = false	// otherwise just load the data into memory
	val ppdbSep = " ||| "	// in input files, not in redis' values
	val loadPPDBfeatures = false	// they are memory intensive, and difficult to make use of in features
	
	@transient
	private var redis: Jedis = null

	@transient
	private var redisCache: collection.Map[String, Seq[PPDBRule]] = null
	
	override def name = "RedisPPDB"
	
	override def setup(calibrateOn: java.util.Collection[DocAlignment]) {
		if(useRedis) {
			println("[RedisPPDB] connecting to redis at %s:%d".format(
				RedisStuff.hostname, RedisStuff.port))
			redis = new Jedis(RedisStuff.hostname, RedisStuff.port)
			if(redis.ping != "PONG")
				throw new RuntimeException("cannot connect to redis! " + RedisStuff.toString)
			println("[RedisPPDB] connection successful")
			redis.select(1)	// see RedisEnglishPPDBSetup
		}
		else loadRedisCacheFromFile
	}

	private def loadRedisCacheFromFile {
		val cacheF = ParmaConfig.getFile("features.ppdb.cache")
		if(cacheF.isFile)
			getRedisCache(cacheF)
		else {
			val t = System.currentTimeMillis
			val f = ParmaConfig.getFile("features.ppdb.redis.file.lexical")
			val g = ParmaConfig.getFile("features.ppdb.redis.file.lexical-self")
			var nr = 0
			println("[RedisPPDB] reading rules from %s and %s".format(f.getPath, g.getPath))
			def gis(f: File) = {
				var is = new FileInputStream(f)
				if(f.getName.toLowerCase.endsWith(".gz"))
					new GZIPInputStream(is)
				else is
			}
			var last = t
			val lines = Source.fromInputStream(gis(f)).getLines ++ Source.fromInputStream(gis(g)).getLines
			redisCache = lines.map(l => {
				nr += 1
				last = RedisIO.printTiming(nr, t, last)
				PPDBRuleIO.line2rule(l, loadPPDBfeatures)
			}).toSeq.groupBy(_.lhs)
			println("[RedisPPDB] read in %d rules for %d types in %.1f seconds"
				.format(nr, redisCache.size, (System.currentTimeMillis-t)/1000d))

			putRedisCache(cacheF)
		}
	}

	import java.util.zip._
	private def getRedisCache(f: File) {
		println("[getRedisCache] reading from " + f.getPath)
		val t = Profiler.getTime {
			val dis = new DataInputStream(new GZIPInputStream(new FileInputStream(f)))
			val buf = new ArrayBuffer[(String, Seq[PPDBRule])]
			val ruleBuf = new ArrayBuffer[PPDBRule]
			val n = dis.readInt
			var i = 0
			while(i < n) {
				val k = dis.readUTF
				val m = dis.readInt
				var j = 0
				while(j < m) {
					ruleBuf += PPDBRuleIO.read(dis)
					j += 1
				}
				ruleBuf.clear
				buf += ((k, ruleBuf.toSeq))
				i += 1
			}
			dis.close
			redisCache = buf.toMap
		}
		println("[getRedisCache] took %.2f seconds".format(t))
	}
	private def putRedisCache(f: File) {
		println("[putRedisCache] writing to " + f.getPath)
		val t = Profiler.getTime {
			val dos = new DataOutputStream(new GZIPOutputStream(new FileOutputStream(f)))
			dos.writeInt(redisCache.size)
			for((k,vs) <- redisCache) {
				dos.writeUTF(k)
				dos.writeInt(vs.size)
				vs.foreach(PPDBRuleIO.write(_, dos))
			}
			dos.close
		}
		println("[putRedisCache] took %.2f seconds".format(t))
	}

	private[this] val binarizer = new FixedWidthBinarizer(25, false, -28d, -4d)
	private[this] val emptyFeatures = DVec.rep(0d/*binarizer.agnostic*/, 7)
	private def featuresFromRule(rule: PPDBRule): DVec = {
		implicit def ppdbweight2double(s: String): Double = {
			val v = java.lang.Double.parseDouble(s)
			assert(v >= 0d)
			math.exp(-v)
		}
		val dv = new DVec(
			rule.properties("p(LHS|f)"),
			rule.properties("p(e|LHS)"),
			rule.properties("p(e|f)"),
			rule.properties("p(e|f,LHS)"),
			rule.properties("p(f|LHS)"),
			rule.properties("p(f|e)"),
			rule.properties("p(f|e,LHS)")
		)
		assert(dv.dimension == emptyFeatures.dimension)
		dv
	}

	var q = 0
	var qAtLeastOneHit = 0
	var qAtLeastTwoHits = 0
	val qStep = 5000

	@transient
	lazy val ppdbKeys = RedisBloomFilter.initialize(true)
	val useCache = false	// cache calls over the network to PPDB, not this.redisCache

	private def getPPDBResults(key: String): Seq[PPDBRule] = {
		if(useRedis) {
			import collection.JavaConversions._
			val maxRHSs = 1000
			if (useCache && !ppdbKeys.in(key)) Seq()
			else redis.lrange(key, 0, maxRHSs).map(PPDBRuleIO.str2rule)
		}
		else
			redisCache.getOrElse(key, Seq())
	}

	private def queryRedis(a: String, b: String, featureName: String, aPOS: String, bPOS: String) {

		throw new RuntimeException("this code has not been updated, simper code is in featurize()")

		val start = System.currentTimeMillis
		val key = a// RedisStuff.makeKey(a, b)
		var rulesProcessed = 0
		var qi = 0
		val hits = new ArrayBuffer[PPDBRule]
		val hitsPOS = new ArrayBuffer[PPDBRule]
		var sumFeatures: DVec = null
		var maxFeatures: DVec = null
		for(rule <- getPPDBResults(key)) {
			rulesProcessed += 1
			if(rule.rhs == b) {
				if(q % qStep == 0)
					println("match! q=%d, a=%s, b=%s, rule=%s".format(q, a, b, rule))
				val features = featuresFromRule(rule)
				if(sumFeatures == null) {
					sumFeatures = features.copy
					maxFeatures = features.copy
				} else {
					sumFeatures += features
					maxFeatures.maxEquals(features)
				}
				hits += rule
				if(rule.parent == aPOS && aPOS == bPOS)
					hitsPOS += rule
			}
		}

		if(sumFeatures == null) sumFeatures = emptyFeatures
		for(i <- 0 until sumFeatures.dimension)
			featureIndexer.addUnstable("hits-sum"+i.toString, sumFeatures(i))
		if(maxFeatures == null) maxFeatures = emptyFeatures
		for(i <- 0 until maxFeatures.dimension)
			featureIndexer.addUnstable("hits-max"+i.toString, maxFeatures(i))

		featureIndexer.addUnstable("numHits", hits.size.toDouble / 10d)
		featureIndexer.addUnstable("numHits1", bool2value(hits.size >= 1))
		featureIndexer.addUnstable("numHits2", bool2value(hits.size >= 2))
		featureIndexer.addUnstable("numHits3", bool2value(hits.size >= 4))
		featureIndexer.addUnstable("numHits4", bool2value(hits.size >= 8))
		featureIndexer.addUnstable("numHits5", bool2value(hits.size >= 16))
		featureIndexer.addUnstable("numHits6", bool2value(hits.size >= 32))
		featureIndexer.addUnstable("numHits7", bool2value(hits.size >= 64))


		// filter out PPDB matches that don't match the POS in these mentions
		featureIndexer.addUnstable("numHits-POS", hitsPOS.size.toDouble / 5d)
		featureIndexer.addUnstable("numHits-POS1", bool2value(hitsPOS.size >= 1))
		featureIndexer.addUnstable("numHits-POS2", bool2value(hitsPOS.size >= 2))
		featureIndexer.addUnstable("numHits-POS3", bool2value(hitsPOS.size >= 4))


		// filter based on PPDB score, count how many hits are left
		for(cutoff <- 2d to 6d by 0.2d map { x => x*x }) {
			val c = hits.filter(_.score < cutoff).size
			val cp = hitsPOS.filter(_.score < cutoff).size
			featureIndexer.addUnstable("hits<%.1f".format(cutoff), c / 2d)
			featureIndexer.addUnstable("hits<%.1f-POS".format(cutoff), cp.toDouble)
		}


		val (f1dv, f2dv) =
			if(hits.size > 0) {
				qAtLeastOneHit += 1
				if(hits.size > 1)
					qAtLeastTwoHits += 1
				val l = hits.minBy(_.properties("p(e|f)").toDouble)
				val r = hits.minBy(_.properties("p(f|e)").toDouble)
				val ldv = featuresFromRule(l)
				val rdv = featuresFromRule(r)
				(DVec.max(ldv, rdv), DVec.sum(ldv, rdv))
			}
			else (emptyFeatures, emptyFeatures)
		for(i <- 0 until f1dv.dimension)
			featureIndexer.addUnstable("p(e|f)"+i.toString, f1dv(i))
		for(i <- 0 until f2dv.dimension)
			featureIndexer.addUnstable("p(f|e)"+i.toString, f2dv(i))


		if(q % qStep == 0) {
			val free = Runtime.getRuntime.freeMemory / 1024f / 1024f
			val maxmem = Runtime.getRuntime.maxMemory / 1024f / 1024f
			println("[RedisPPDB featurize] free=%.1f MB, maxMem=%.1f MB".format(free, maxmem))
			println("[RedisPPDB featurize] q=%d qAtLeastOneHit=%d, qAtLeastTwoHits=%d, a=%s, b=%s, %d rules processed in %d ms"
				.format(q, qAtLeastOneHit, qAtLeastTwoHits, a, b, rulesProcessed, System.currentTimeMillis - start))
		}
		q += 1
	}

	case class PPDBRule2(val lhs: String, val rhs: String, val parent: String, val logProb: Double) { require(logProb < 0d) }
	object PPDBRule2 {
		def fromRedis(str: String): PPDBRule2 = {
			val ar = str.split("\t")
			PPDBRule2(ar(0), ar(1), ar(2), -1d * ar(3).toDouble)
		}
	}

	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {

		// TODO max features over all mentions
		val (rCM, pCM) = CanonicalMentionFinder.canonicalMentions(a, report, passage)
		val rsent = report.getSentence(rCM)
		val psent = passage.getSentence(pCM)

		// [1] mention head word
		val rw: Token = report.getHeadToken(rCM)
		val pw: Token = passage.getHeadToken(pCM)
		val rk: String = rw.getWord
		val pk: String = pw.getWord
		val maxRHSs = 250
		val rRules =
			if(useRedis) {
				redis.lrange(rk, 0, maxRHSs)
					.map(PPDBRuleIO.str2rule)
					//.map(PPDBRule2.fromRedis)
			}
			else redisCache.getOrElse(rk, new ArrayBuffer[PPDBRule])
		val rMatches: Seq[(PPDBRule, Int)] = rRules.zipWithIndex.filter(_._1.rhs == pk)
		// i'm going to assume PPDB is symmetric => no pRules or pMatches

		if(rMatches.size == 0) b(sv, "noMatches")
		if(rMatches.size > 1) b(sv, "atLeastOneMatch")
		if(rMatches.size > 2) b(sv, "atLeastTwoMatches")
		val v: Double = rMatches.headOption match {
			case Some((rule, idx)) => idx
			case None => (rRules.size + maxRHSs) / 2d
		}
		for(cutoff <- Seq(2, 4, 8, 15, 25, 50, 75, 125, 200, 300))
			if(v <= cutoff) b(sv, "best-index-lt" + cutoff)
		b(sv, rMatches.headOption match {
			case Some((rule, idx)) => rule.score //logProb
			case None => -30d	// make sure this is below the binarizer scale
		}, "best-logProb", binarizer)





		// [2] mention head lemma
		//val rMention = report.getMentionSpan(rCM).map(_.getLemma).mkString(" ")
		//val pMention = passage.getMentionSpan(pCM).map(_.getLemma).mkString(" ")
		//queryRedis(rMention, pMention, "mention", addTo)
		//queryRedis(pMention, rMention, "mention", addTo)

		/*
		// [3] 1..k word phrases to the left of both mentions
		val rLeft = rsent.before(rCM)
		val pLeft = psent.before(pCM)
		for(i <- 1 to 3) {
			val rls = rLeft.takeRight(i).map(_.getWord).mkString(" ")
			val pls = pLeft.takeRight(i).map(_.getWord).mkString(" ")
			queryRedis(rls, pls, "left"+i, addTo)
		}

		// [4] 1..k word phrases to the right of both mentions
		val rRight = rsent.after(rCM)
		val pRight = psent.after(pCM)
		for(i <- 1 to 3) {
			val rrs = rRight.take(i).map(_.getWord).mkString(" ")
			val prs = pRight.take(i).map(_.getWord).mkString(" ")
			queryRedis(rrs, prs, "right"+i, addTo)
		}

		// [5] 1..k word phrases on both sides of both mentions
		for(i <- 1 to 3) {
			val r = rsent.slice(rCM.getStartTokenIdx - i, rCM.getEndTokenIdx + i).map(_.getWord).mkString(" ")
			val p = psent.slice(pCM.getStartTokenIdx - i, pCM.getEndTokenIdx + i).map(_.getWord).mkString(" ")
			queryRedis(r, p, "overlap"+i, addTo)
		}
		*/
	}

	
}



// code needed to insert into redis
object RedisPPDBSetup {
	val ppdbKeys = RedisBloomFilter.initialize(true)

	def main(args: Array[String]) {
		if(args.length != 1) {
			println("please provide a parma.config file")
			sys.exit(-1)
		}
		ParmaConfig.load(args(0))
		println("[redis insert] trying to connect to redis at %s on port %d".format(RedisStuff.hostname, RedisStuff.port))
		val redis = new Jedis(RedisStuff.hostname, RedisStuff.port)
		// load redis db 1 instead of db 0
		redis.select(1)
		val f = ParmaConfig.getFile("features.ppdb.redis.file.lexical")
		val reader = FileUtils.getReader(f)

		//println("[redis insert] flushing existing stuff...")
		//val t = redis.multi
		//t.flushDB
		//t.exec
		println("for some reason, cannot flush programatically, do so manually")

		println("[redis insert] inserting stuff...")
		//val pipe = redis.pipelined
		var i = 0
		val start = System.currentTimeMillis
		var last = start
		while(reader.ready) {
			val line = reader.readLine
			val ar = line.split(" \\|\\|\\| ")
			val score = ar(0).toDouble
			val parent = ar(1).toLowerCase
			val leftChild = ar(2).toLowerCase
			val rightChild = ar(3).toLowerCase
			val properties = str2map(ar(ar.length-2))

			val key = leftChild //+"|"+ rightChild
			val value = PPDBRuleIO.rule2str(new PPDBRule(leftChild, rightChild, parent, score, properties))
			//val value = List(rightChild, properties("p(LHS|e)"), properties("p(LHS|f)"),
			//				properties("p(e|LHS)"), properties("p(e|f)"), properties("p(e|f,LHS)"),
			//				properties("p(f|LHS)"), properties("p(f|e)"), properties("p(f|e,LHS)"))
			//println("key = %s, value = [%s]".format(key, value))
			//redis.rpush(key, value: _*)
			redis.rpush(key, value)
			ppdbKeys.set(key)
			//pipe.rpush(key, value)

			val step = 15000
			if(i % step == 0) {
				val now = System.currentTimeMillis
				val recent = step.toDouble / (now-last)
				val avg = i.toDouble / (now-start)
				println("i=%d, %.1f K lines/sec recent, %.1f K lines/sec avg".format(i, recent, avg))
				//println("adding(%d) %s => %s (%.1f %.1f K lines/sec recent/avg)".format(i, key, properties, recent, avg))
				last = now
			}
			i += 1
		}
		reader.close
		ppdbKeys.write
		//pipe.exec

		println("done, added %d rules in %.1f minutes".format(i, (System.currentTimeMillis - start)/(60d*1000d)))
	}

	private def str2map(kvs: String): Map[String, String] = {
		val properties = kvs.split(" ").flatMap(kv => {
			val x = kv.split("=")
			if(x.length != 2) {
				//println("kvs = " + kvs)
				//throw new RuntimeException("x = [%s]".format(x.mkString(", ")))
				Seq()
			}
			else Seq((x(0), x(1)))
		}).toMap

		// get rid of trailing 0s
		properties.mapValues(value => """\.0*$""".r.replaceAllIn(value, ""))
	}

}


object RedisPPDBTest {
	val maxRHSs = 5
	def main(args: Array[String]) {
		ParmaConfig.load("parma.config")
		val redis = new Jedis(RedisStuff.hostname, RedisStuff.port)
		for(word <- Seq("dog", "said", "ran", "eat", "car", "capital", "building")) {
			println("\nquery = " + word)
			println(redis.lrange(word, 0, maxRHSs).map(PPDBRuleIO.str2rule).mkString("\n"))
		}
	}
}



/* vim: set noet : */
