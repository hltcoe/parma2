package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.feature_interfaces.AlignmentSimilarity
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder
import edu.jhu.hlt.parma.util.SHA1er
import java.security.MessageDigest
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import redis.clients.jedis._

/**
 * Insert English PPDB into Redis
 */
object RedisEnglishPPDBSetup {
	def main(args: Array[String]) {
    if(args.length < 1 || args.length > 2) {
      println(""" | Usage: java RedisEnglishPPDBSetup [config-file] ([flush])
                  | If the second argument, flush, is given then this data overwrites
                  | the existing database.  Otherwise this data is appended to the database.
              """.stripMargin)
			sys.exit(-1)
		}
		ParmaConfig.load(args(0))

		// travis: this was true, but appears to be reading in something when i want to make a new bloom filter?
		val ppdbKeys = RedisBloomFilter.initialize(false)

		println("[redis insert] trying to connect to redis at %s on port %d".format(RedisStuff.hostname, RedisStuff.port))
		val jedis = new Jedis(RedisStuff.hostname, RedisStuff.port)
		val f = ParmaConfig.getFile("features.ppdb.redis.file.lexical")
		val s = ParmaConfig.getFile("features.ppdb.redis.file.lexical-self")
		val reader = FileUtils.getReader(f)
		val selfReader = FileUtils.getReader(s)

		// flush if desired
		if (args.length == 2 && args(1) == "flush") {
			jedis.select(0)
			jedis.flushDB
			assert(jedis.dbSize == 0)
			println("Lexical + self flushed")
			jedis.select(1)
			jedis.flushDB
			assert(jedis.dbSize == 0)
			println("Lexical only flushed")
		}

		println("[redis insert] inserting stuff...")
		var i = 0
		val start = System.currentTimeMillis
		var last = start
		//non-identity rules into db 0 and db 1
		println("[redis insert] Non-identity rules...")
		while(reader.ready) {
			val line = reader.readLine
			val (leftKey, bothKey, value) = RedisIO.processPPDBLine(line)
			jedis.select(0)
			jedis.rpush(leftKey, value)
			jedis.rpush(bothKey, value)
			jedis.select(1)
			jedis.rpush(leftKey, value)
			ppdbKeys.set(leftKey)
			ppdbKeys.set(bothKey)

			last = RedisIO.printTiming(i, start, last)
			i += 1

		}
		reader.close
		println("mostly done, added %d rules in %.1f minutes".format(i, (System.currentTimeMillis - start)/(60d*1000d)))

		//identity rules into db 0
		println("[redis insert] Identity rules...")
		jedis.select(0)
		while(selfReader.ready) {
			val line = selfReader.readLine
			val (leftKey, bothKey, value) = RedisIO.processPPDBLine(line)
			jedis.rpush(leftKey, value)
			jedis.rpush(bothKey, value)
			ppdbKeys.set(leftKey)
			ppdbKeys.set(bothKey)

			last = RedisIO.printTiming(i, start, last)
			i += 1
		}
		selfReader.close
		ppdbKeys.write

		println("done, added %d rules in %.1f minutes".format(i, (System.currentTimeMillis - start)/(60d*1000d)))
	}
}


/**
 * Insert Spanish PPDB into Redis
 */
object RedisSpanishPPDBSetup {
	def main(args: Array[String]) {
    if(args.length < 1 || args.length > 2) {
      println(""" | Usage: java RedisSpanishPPDBSetup [config-file] ([flush])
                  | If the second argument, flush, is given then this data overwrites
                  | the existing database.  Otherwise this data is appended to the database.
              """.stripMargin)
			sys.exit(-1)
		}
		ParmaConfig.load(args(0))
		val ppdbKeys = RedisBloomFilter.initialize(true)
	
		println("[redis insert] trying to connect to redis at %s on port %d".format(RedisStuff.hostname, RedisStuff.port))
		val jedis = new Jedis(RedisStuff.hostname, RedisStuff.port)
		val f = ParmaConfig.getFile("features.ppdb.redis.file.spanish.lexical")
		val reader = FileUtils.getReader(f)

		// flush if desired
		if (args.length == 2 && args(1) == "flush") {
			jedis.select(4)
			jedis.flushDB
			assert(jedis.dbSize == 0)
			println("Spanish PPDB flushed")
		}

		jedis.select(4)
		println("[redis insert] inserting stuff...")
		var i = 0
		val start = System.currentTimeMillis
		var last = start
		//non-identity rules into db 0 and db 1
		println("[redis insert] Non-identity rules...")
		while(reader.ready) {
			val line = reader.readLine
			val (leftKey, bothKey, value) = RedisIO.processPPDBLine(line)
			jedis.rpush(leftKey, value)
			ppdbKeys.set(leftKey)

			last = RedisIO.printTiming(i, start, last)
			i += 1
		}
		reader.close

		ppdbKeys.write

		println("done, added %d rules in %.1f minutes".format(i, (System.currentTimeMillis - start)/(60d*1000d)))
	}
}


/**
 * Insert English-Spanish and Spanish-English Joshua phrase-tables into Redis
 */
object RedisJoshuaSetup {
	def main(args: Array[String]) {
		if (args.length != 2 && args.length != 3) {
			println("""| Usage: java RedisJoshuaSetup [config.file] [query-language] ([flush])
								 | e.g. java RedisJoshuaSetup parma.config english flush
								 | or		java RedisJoshuaSetup parma.config spanish
								 """.stripMargin)
			sys.exit(-1)
		}
		ParmaConfig.load(args(0))
		val ppdbKeys = RedisBloomFilter.initialize(true)

		val queryLanguage = args(0)
		val redisKeys = RedisBloomFilter.initialize(true)
		println("[redis insert] trying to connect to redis at %s on port %d".format(RedisStuff.hostname, RedisStuff.port))
		val jedis = new Jedis(RedisStuff.hostname, RedisStuff.port)
		
		// load appropriate redis db 
		val (joshuaFile, dbNumber) = queryLanguage match {
			case "english" => (ParmaConfig.getFile("features.joshua.redis.file.english-spanish"), 2)
			case "spanish" => (ParmaConfig.getFile("features.joshua.redis.file.spanish-english"), 3)
			case _ => { println("couldn't understand query-language, using English")
									(ParmaConfig.getFile("features.joshua.redis.file.english-spanish"), 2) }
		}
		jedis.select(dbNumber)
		// flush if desired
		if (args.length == 3 && args(2) == "flush") {
			jedis.flushDB
			assert(jedis.dbSize == 0)
			println("db "+dbNumber+" flushed")
		} 

		val reader = FileUtils.getReader(joshuaFile)

		println("[redis insert] inserting stuff...")
		//val pipe = redis.pipelined
		var i = 0
		val start = System.currentTimeMillis
		var last = start
		while(reader.ready) {
			val line = reader.readLine
			val (key, value) = RedisIO.processJoshuaLine(line)

			jedis.rpush(key, value)
			redisKeys.set(key)

			last = RedisIO.printTiming(i, start, last)
			i += 1
		}
		reader.close
		redisKeys.write

		println("done, added %d rules in %.1f minutes".format(i, (System.currentTimeMillis - start)/(60d*1000d)))
	}
}


/**
 * Wrapper for redis host and port
 */
object RedisStuff {
	val hostname = ParmaConfig.getString("features.ppdb.redis.host")
	val port = ParmaConfig.getInt("features.ppdb.redis.port")

	override def toString: String = "(RedisStuff hostname=%s port=%d)".format(hostname, port)
	
	/**
	 * redis likes you to give keys that are already hashed, more efficient
	 */
	def makeKey(parts: String*): String = parts.mkString(":")
}


/**
 * PPDBRule class
 */
class PPDBRule(val lhs: String, val rhs: String, val parent: String, val score: Double, val allProperties: scala.collection.Map[String, String]) {
	val neededKeys = Set("p(LHS|e)", "p(LHS|f)", "p(e|LHS)", "p(e|f)", "p(e|f,LHS)", "p(f|LHS)", "p(f|e)", "p(f|e,LHS)")
	val properties = allProperties.filterKeys(k => neededKeys.contains(k))

	lazy val logProb = neededKeys.map{ k => -allProperties(k).toDouble }.sum
	override def toString = PPDBRuleIO.rule2str(this)
}


/**
 * JoshuaRule class
 */
case class JoshuaRule(val lhs: String, val rhs: String, val score: Double)


/**
 * PPDB specific helper methods
 */
object PPDBRuleIO {
	import scala.collection.mutable.HashMap
	import java.io._
	def write(rule: PPDBRule, dos: DataOutputStream) {
		dos.writeUTF(rule.lhs)
		dos.writeUTF(rule.rhs)
		dos.writeUTF(rule.parent)
		dos.writeDouble(rule.score)
		dos.writeInt(rule.allProperties.size)
		for((k,v) <- rule.allProperties) {
			dos.writeUTF(k)
			dos.writeUTF(v)
		}
	}
	def read(dis: DataInputStream): PPDBRule = {
		val lhs = dis.readUTF
		val rhs = dis.readUTF
		val parent = dis.readUTF
		val score = dis.readDouble
		val n = dis.readInt
		val buf = new ArrayBuffer[(String, String)]
		for(i <- 0 until n) {
			val k = dis.readUTF
			val v = dis.readUTF
			buf += ((k, v))
		}
		new PPDBRule(lhs, rhs, parent, score, buf.toMap)
	}

	// TODO fix this, only tab is supported now
	val sep1 = "\t"
	val sep2 = "\t"
	val eq = "\t"

	val noFeatures = Map[String, String]()

	/** use this when reading from a PPDB file */
	def line2rule(line: String, loadFeatures: Boolean = true): PPDBRule = {
		val ar = line.split(" \\|\\|\\| ")
		require(ar.length == 6)
		val score = -1d * ar(0).toDouble
		val parent = ar(1).intern
		val lhs = ar(2).intern
		val rhs = ar(3).intern
		if(loadFeatures) {
			val features = ar(4)
			val iDontKnow = ar(5)
			val featureMap = features.split("\\s+").map(s => {
				val a = s.split("=")
				require(a.length == 2)
				(a(0), a(1))
			}).toMap + ("idk"->iDontKnow)
			new PPDBRule(lhs, rhs, parent, score, featureMap)
		}
		else new PPDBRule(lhs, rhs, parent, score, noFeatures)
	}

	/** use this when pulling values from Redis */
	def str2rule(redisStr: String): PPDBRule = {
		val ar = redisStr.split("\t")
		val lhs = ar(0)
		val rhs = ar(1)
		val parent = ar(2)
		val score = ar(3).toDouble
		val props = new HashMap[String, String]
		assert(ar.length % 2 == 0)
		for(i <- 4 until ar.length by 2)
			props.update(ar(i), ar(i+1))
		new PPDBRule(lhs, rhs, parent, score, props)
	}

	def validToken(s: String) = s.indexOf(sep1) < 0 && s.indexOf(sep2) < 0 && s.indexOf(eq) < 0

	private def sortedKVs(rule: PPDBRule): String = rule.properties.map(kv => kv._1 + eq + kv._2).toBuffer.sorted.mkString(sep2)

	def rule2str(rule: PPDBRule): String = {
		if(!validToken(rule.lhs))
			println("bad rule.lhs: " + rule.lhs)
		if(!validToken(rule.rhs))
			println("bad rule.rhs: " + rule.rhs)
		assert(validToken(rule.lhs))
		assert(validToken(rule.rhs))
		assert(validToken(rule.parent))
		rule.properties.foreach(kv => {
			if(!validToken(kv._1) || !validToken(kv._2))
				println("bad: " + kv)
			assert(validToken(kv._1))
			assert(validToken(kv._2))
		})
		List(rule.lhs, rule.rhs, rule.parent, rule.score.toString, sortedKVs(rule)).mkString(sep1)
	}
}


/**
 * Other Redis helper methods
 */
object RedisIO {
	def printTiming(i: Long, start: Long, last: Long): Long = {
		val step = 15000
		val next = if(i % step == 0) {
			val now = System.currentTimeMillis
			val recent = step.toDouble / (now-last)
			val avg = i.toDouble / (now-start)
			println("i=%d, %.1f K lines/sec recent, %.1f K lines/sec avg".format(i, recent, avg))
			now
		} else last
		next
	}

	def processPPDBLine(line: String): (String, String, String) = {
		val ar = line.split(" \\|\\|\\| ")
		val score = ar(0).toDouble
		val parent = ar(1).trim.toLowerCase
		val leftChild = ar(2).trim.toLowerCase
		val rightChild = ar(3).trim.toLowerCase
		val properties = str2map(ar(4))

		val bothKey = leftChild +"|"+ rightChild
		val value = PPDBRuleIO.rule2str(new PPDBRule(leftChild, rightChild, parent, score, properties))
		(leftChild, bothKey, value)
	}

	def processJoshuaLine(line: String): (String, String) = {
			val ar = line.split(" \\|\\|\\| ")
			val leftChild = ar(0)
			val rightChild = ar(1)
			val score = ar(2).toDouble
			val value = joshRule2str(new JoshuaRule(leftChild, rightChild, score))
			(leftChild, value)
	}

	def str2map(kvs: String): Map[String, String] = {
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

	def joshRule2str(rule: JoshuaRule): String = {
    rule.lhs +"\t"+ rule.rhs + "\t" + rule.score.toString
  }
}


