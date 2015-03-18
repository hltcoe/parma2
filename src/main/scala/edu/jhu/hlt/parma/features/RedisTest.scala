// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

//import com.redis._
import redis.clients.jedis._

class RedisTest {

    def foo {
        //val r = new RedisClient("localhost", 1000)
        //r.sadd("foo", "bar")
		val jedis = new Jedis("localhost");
		jedis.set("foo", "bar")
		println(jedis.get("foo"))
    }

}

