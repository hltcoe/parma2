// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util

import collection.JavaConversions._
import java.util.NoSuchElementException

class Alphabet[T] extends Serializable {

    private[this] val t2i = new java.util.HashMap[T, Integer]
    private[this] val i2t = new java.util.ArrayList[T]
    private[this] var growthStopped = false

	def keys: Seq[T] = i2t.toSeq
	def indices: Seq[Int] = (0 until size)
	def apply(t: T): Int = lookupIndex(t)
	def apply(i: Int): T = lookupObject(i)
	def contains(t: T): Boolean = t2i.containsKey(t)

    def lookupObject(i: Int): T = {
        if(i < 0) throw new RuntimeException("need non-negative indices: " + i)
        if(i >= i2t.size) throw new NoSuchElementException("that index hasn't been assigned: " + i)
        return i2t.get(i)
    }

    def lookupIndex(t: T, addIfNotPresent: Boolean = false): Int = {
        val i = t2i.get(t)
        if(i == null) {
            if(addIfNotPresent) {
				i2t.add(t)
                t2i.put(t, t2i.size)
                return t2i.size - 1
            }
            else throw new NoSuchElementException(t + " does not exist!")
        }
        else return i
    }

	def size: Int = {
		assert(i2t.size == t2i.size)
		i2t.size
	}

	def clear {
		t2i.clear
		i2t.clear
	}

}

