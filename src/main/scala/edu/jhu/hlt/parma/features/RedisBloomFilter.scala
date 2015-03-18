// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import collection.JavaConversions._
import redis.clients.jedis._

class RedisBloomFilter 
object RedisBloomFilter {
	import edu.jhu.jerboa.counting.BloomFilter
	import edu.jhu.jerboa.util.JerboaProperties
	import edu.jhu.hlt.parma.util.ParmaConfig

	
	def initialize(fromFile: Boolean = true): BloomFilter = {
		JerboaProperties.load(ParmaConfig.getPropertiesFileName)
		@transient
		lazy val jedis = new Jedis(RedisStuff.hostname, RedisStuff.port)
		if(jedis.ping != "PONG") {
			throw new RuntimeException("cannot connect to redis! " + RedisStuff.toString)
		}
		//lexical PPDB has 329799 LHSs in db 0 6654262 paraphrases in 1
		lazy val redisLHS = {jedis.select(0); jedis.dbSize}
		lazy val redisPairs = {jedis.select(1); jedis.dbSize}
		lazy val redisEntries = redisLHS + redisPairs // 6984059
		val errorRate = 0.00001
		val memory = BloomFilter.memoryNeeded(redisEntries, errorRate) //167738521 ~= 160M
		val redisKeys = new BloomFilter(memory, redisEntries)
		if (fromFile) redisKeys.read
		else 	{
			//this breaks with large keysets, best to initialize from a file
			jedis.select(0)
			jedis.keys("*").foreach(redisKeys.set(_))
			jedis.select(1)
			jedis.keys("*").foreach(redisKeys.set(_))
			redisKeys.write
		}
		redisKeys
	}
}

