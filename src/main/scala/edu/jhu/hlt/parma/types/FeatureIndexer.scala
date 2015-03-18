// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.types

import edu.jhu.hlt.parma.util.Binarizer
import cc.mallet.types.Alphabet
import scala.collection.mutable.ArrayBuffer

/**
 * this class maintains an index between named features (values, not functions)
 * and a dense/sparse index. this utilizes mallet's alphabets, but is more efficient
 * in cases where the exact same features are called in succession each time.
 *
 * see this class in use in AlignmentSimilarityFunction.featurize
 * this class is used by HierarchicalAlignmentModule.computeFeatures
 *
 * if you can gaurantee that your feature function will call addStable() with
 * the same keys every time (i.e. sequence of add calls between start() and commit()),
 * then use addStable(), which requires only an ArrayBuffer append
 *
 * addUnstable is for when the same keys don't always appear in the same order
 * e.g. with sparse features, where you might have keys named "contains-%s".
 * this method costs a hashmap lookup every time, but is more safe
 *
 * note that if you want to use addStable(), the keys must not only be the same,
 * the number of calls must be the same. this often means doing aggregation
 * yourself instead of multiple calls to add*(). e.g.
 * WRONG: for(w in document): if(predicate(w)): addStable("featureName", 1.0)
 * RIGHT: addStable("featureName", document.filter(predicate).size)
 *
 * use maxIdx so that compound indices know how many bits to allocate for the
 * low-order bits.
 */
sealed class FeatureIndexer[K](val maxIdx: Int = 1024) extends Serializable {

	private[this] var curSV: SVec = null
	private[this] var starts = 0
	private[this] var commits = 0
	private[this] var stableIdx: Int = 0
	private[this] val stableAlph = new Alphabet
	private[this] val unstableAlph = new Alphabet

	// unstable indices must be offset by the #dense-keys
	// on the first pass, when we don't know #dense-keys,
	// we need to store the values until commit()
	private[this] var firstPassUnstableSV = new SVec

	private[this] var numStableKeys = -99999	// set after first run

	def start(sv: SVec) {
		stableIdx = 0
		curSV = sv
		assert(starts == commits)
		starts = starts + 1
	}

	def addStable(key: K, value: Double) {
		if(starts == 1) {
			val idx = stableAlph.lookupIndex(key, true)
			assert(idx == stableIdx, "alph=%s key=%s idx=%d stableIdx=%d".format(stableAlph, key, idx, stableIdx))
		}
		curSV.add(stableIdx, value)
		stableIdx = stableIdx + 1
	}

	def addStable(key: K, value: Double, binarizer: Binarizer) {
		if(starts == 1) {
			for(i <- 0 until binarizer.numBuckets)
				stableAlph.lookupIndex(key + ":" + i.toString, true)
		}
		val bucket = binarizer.binarize(value)
		if(binarizer.useBucketRanges)
			(0 to bucket).foreach(i => curSV.add(i, 1d))
		else
			curSV.add(stableIdx + bucket, 1d)
		stableIdx = stableIdx + binarizer.numBuckets
	}

	def addUnstable(key: K, value: Double) {
		if(value.isNaN || value.isInfinite)
			throw new RuntimeException("%s = %.3f".format(key, value))
		val idx = unstableAlph.lookupIndex(key, true)
		if(starts > 1) {
			assert(numStableKeys >= 0, "start=%d numStableKeys=%d".format(starts, numStableKeys))
			curSV.add(idx + numStableKeys, value)
		}
		else firstPassUnstableSV.add(idx, value)
	}

	def commit {
		if(starts == 1) {
			numStableKeys = stableAlph.size
			stableAlph.stopGrowth
			//curSV.addWithOffset(firstPassUnstableSV, stableIdx)
			firstPassUnstableSV.items.foreach(iv => curSV.add(iv._1 + stableIdx, iv._2))
			firstPassUnstableSV = null
		}
		curSV = null
		commits = commits + 1
		assert(starts == commits)
		assert(stableIdx == stableAlph.size, "stableAlph=%s starts=commits=%d stableIdx=%d stableAlph.size=%d"
			.format(stableAlph, starts, stableIdx, stableAlph.size))
	}

	def lookupIndex(key: K): Int = {
		val i = stableAlph.lookupIndex(key)
		if(i >= 0) {
			assert(unstableAlph.lookupIndex(key) < 0, "ambiguous key: " + key)
			return i
		} else {
			val j = unstableAlph.lookupIndex(key)
			if(j < 0) throw new RuntimeException("key not found: " + key)
			return j
		}
	}

	def lookupObject(index: Int): K = {
		assert(starts > 0)
		assert(index >= 0 && index < numStableKeys + unstableAlph.size,
			"index=%d numStableKeys=%d unstableAlph.size=%d".format(index, numStableKeys, unstableAlph.size))
		val x = 
			if(index < numStableKeys)
				stableAlph.lookupObject(index)
			else
				unstableAlph.lookupObject(index - numStableKeys)
		x.asInstanceOf[K]
	}

}



