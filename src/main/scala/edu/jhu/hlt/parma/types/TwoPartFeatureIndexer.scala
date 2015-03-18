// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.types

import edu.jhu.hlt.parma.util.Describe
import cc.mallet.types.Alphabet

/**
 * wraps an alphabet for storing an Object<=>Index bijection
 * and provides a methods for packing and unpacking SVecs
 */
class TwoPartFeatureIndexer[K](val maxOuterKeys: Int, val maxInnerKeys: Int) extends Serializable {

	val outerBits = bits(maxOuterKeys)
	val innerBits = bits(maxInnerKeys)
	val outerMask = mask(outerBits, innerBits)
	val innerMask = mask(innerBits, 0)
	val outerAlph = new edu.jhu.hlt.parma.util.Alphabet[K]

	require(innerBits + outerBits <= 31,
		"too many bits to fit in an int! maxOuterKeys=%d maxInnerKeys=%d"
			.format(maxOuterKeys, maxInnerKeys))

	def bits(i: Int): Int = {
		val le2 = math.log(2d)
		math.ceil(math.log(i) / le2).toInt
	}

	def mask(numBits: Int, offset: Int = 0): Int = ((1 << numBits) - 1) << offset

	assert(Integer.toBinaryString(mask(bits(5), 2)) == "11100")
	assert(Integer.toBinaryString(mask(bits(64))) == "111111")

	/**
	 * does not store anything in the alphabet/bijection,
	 * so only use if you know what you're doing.
	 * see other reindex for documentation.
	 */
	def reindex(outerKey: Int, src: SVec, dest: SVec) {
		assert(bits(outerKey+1) <= outerBits)
		src.items.foreach(iv => {
			if(iv._1 >= maxInnerKeys) {
				throw new RuntimeException("you have violated the TwoKeyIndexer contract, " +
					"you said you would only use %d inner indices but you just gave ".format(maxInnerKeys) +
					"a vector containing the index %d (from 0)".format(iv._1))
			}
			val index = (outerKey << innerBits) | iv._1
			dest.add(index, iv._2)
		})
	}

	/**
	 * stores outerKey in the alphabet and adds the
	 * index-corrected elements from src into dest.
	 * does not change src.
	 */
	def reindex(outerKey: K, src: SVec, dest: SVec, addIfNotPresent: Boolean = false) {
		reindex(outerAlph.lookupIndex(outerKey, addIfNotPresent), src, dest)
	}

	def lookupOuterIndex(outerKey: K, addIfNotPresent: Boolean = false): Int = {
		val i = outerAlph.lookupIndex(outerKey, addIfNotPresent)
		if(i < 0) throw new RuntimeException("no such key: " + outerKey)
		return i
	}

	def lookupOuterKey(outerIndex: Int): K = {
		val o = outerAlph.lookupObject(outerIndex)
		if(o == null) throw new RuntimeException("no such index: " + outerIndex)
		return o.asInstanceOf[K]
	}

	def lookupIndex(fullIndex: Int): (K, Int) = {
		val (outerIdx, innerIdx) = lookupIndexRaw(fullIndex)
		(lookupOuterKey(outerIdx), innerIdx)
	}

	def lookupIndexRaw(fullIndex: Int): (Int, Int) = {
		val outerIdx = (fullIndex & outerMask) >>> innerBits
		val innerIdx = fullIndex & innerMask
		(outerIdx, innerIdx)
	}

	def numOuterKeys: Int = outerAlph.size
}

