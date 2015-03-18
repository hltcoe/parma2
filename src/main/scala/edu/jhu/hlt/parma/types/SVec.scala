// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.types

object SVec {
	def apply(i: Int, v: Double): SVec = {
		val sv = new SVec(1)
		sv.add(i, v)
		sv
	}
	def apply(ivs: (Int, Double)*): SVec = {
		val sv = new SVec
		for(iv <- ivs)
			sv.add(iv._1, iv._2)
		sv
	}

	def duplicate(sv: SVec): SVec = duplicate(sv, sv.items.size)
	def duplicate(sv: SVec, newSize: Int): SVec = {
		assert(newSize >= sv.items.size)
		val nsv = new SVec(newSize)
		nsv ++= sv
		nsv
	}

	/**
	 * turns every index `i` in src into `i*stride + offset`
	 * and adds it to dest
	 *
	 * this is useful for some of the feature reindexing
	 * (e.g. DomainAdaptation and FeatureRefiner)
	 */
	def addWithStride(src: SVec, dest: SVec, stride: Int, offset: Int) {
		if(offset >= stride || stride < 1 || offset < 0) {
			throw new IllegalArgumentException("offset must be an index that is " +
				"less than stride: offset=%d stride=%d".format(offset, stride))
		}
		src.items.foreach(iv => dest.add(iv._1 * stride + offset, iv._2))
	}

}

sealed class SVec(initialSize: Int = 32) extends Serializable {

	private[this] var indices = Array.ofDim[Int](initialSize)
	private[this] var values = Array.ofDim[Double](initialSize)
	private[this] var top = 0			// indices less than this are valid
	private[this] var cap = initialSize	// size of current arrays

	/**
	 * WARNING: best not to mess with the backing unless you know what you're doing
	 */
	def getIndices: Array[Int] = indices
	/**
	 * WARNING: best not to mess with the backing unless you know what you're doing
	 */
	def getValues: Array[Double] = values
	/**
	 * WARNING: best not to mess with the backing unless you know what you're doing
	 */
	def getTop: Int = top


	def add(i: Int, v: Double) {
		if(i < 0)
			throw new RuntimeException("you cannot give me a negative index! idx=%d value=%3g".format(i, v))
		if(v == 0d) return
		if(top == cap) {
			cap = (cap * 1.5 + 2).toInt
			indices = java.util.Arrays.copyOf(indices, cap)
			values = java.util.Arrays.copyOf(values, cap)
		}
		indices(top) = i
		values(top) = v
		top += 1
	}

	/**
	 * adds every value in DVec where the indices are interpretted as:
	 * (offset, offset + 1, ..., offset + dv.dimension - 1)
	 */
	def add(dv: DVec, offset: Int) {
		val n = dv.dimension
		var i = 0
		while(i < n) {
			add(offset + i, dv(i))
			i += 1
		}
	}

	def ++=(sv: SVec) {
		sv.items.foreach(iv => add(iv._1, iv._2))
	}

	def clear {
		top = 0
	}

	def rawIndices = indices.toIndexedSeq

	def numItems: Int = top

	// TODO this is bugs waiting to happen... not specified whether you
	// get a compacted view of this vector or not
	def items: Iterator[(Int, Double)] = (0 until top).map(i => (indices(i), values(i))).iterator

	/**
	 * for debugging
	 */
	def containsBadValues(checkForNaN: Boolean = true, checkForInf: Boolean = true): Boolean = {
		def bad(d: Double) = (checkForNaN && d.isNaN) || (checkForInf && d.isInfinite)
		val allGood = items.map(_._2).forall(d => !bad(d))
		!allGood
	}

	def compact {
		val (i, v) = uniqItems
		assert(i.length == v.length)
		indices = i
		values = v
		top = i.length - 1
		cap = i.length
	}

	/**
	 * this doesn't seem to work properly with the rest of
	 * mallet's pipeline/instancelist/training incantation,
	 * and it has to do with needing the keys entered into a
	 * common alphabet...
	 * TODO find a way to do this in mallet without an alphabet
	 */
	def toMallet = {
		compact
		new cc.mallet.types.SparseVector(indices, values)
	}

	// i could just just define apply for SVec, but that seems risky...
	private sealed class IntDoubleAccum(size: Int) extends cern.colt.function.IntDoubleProcedure {
		private[this] var ints = Array.ofDim[Int](size)
		private[this] var doubles = Array.ofDim[Double](size)
		private[this] var i = 0
		override def apply(first: Int, second: Double): Boolean = {
			ints(i) = first
			doubles(i) = second
			i += 1
			true
		}
		def get = (ints, doubles)
	}

	def uniqItems: (Array[Int], Array[Double]) = {
		val u = new cern.colt.map.OpenIntDoubleHashMap(top/2)
		var i = 0
		while(i < top) {
			val idx = indices(i)
			val old = u.get(idx)
			u.put(idx, old + values(i))
			i += 1
		}
		val accum = new IntDoubleAccum(u.size)
		u.forEachPair(accum)
		accum.get
	}

	def lInf: Double = {
		compact
		var m = 0d
		var i = 0
		while(i < top) {
			val v = math.abs(values(i))
			if(v > m) m = v
			i += 1
		}
		m
	}

	def l2: Double = {
		compact
		var ss = 0d
		var i = 0
		while(i < top) {
			val v = values(i)
			ss += v * v
			i += 1
		}
		math.sqrt(ss)
	}

	def l1: Double = {
		compact
		var s = 0d
		var i = 0
		while(i < top) {
			val v = values(i)
			if(v >= 0d) s += v
			else s -= v
			i += 1
		}
		s
	}

	def *=(factor: Double) {
		var i = 0
		while(i < top) {
			values(i) *= factor
			i += 1
		}
	}
}

