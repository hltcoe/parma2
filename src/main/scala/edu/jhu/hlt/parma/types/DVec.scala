// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.types

import edu.jhu.hlt.parma.util.VecOps
import java.util.Arrays

class DVec(private[this] var items: Array[Double]) extends Iterable[(Int, Double)] with Serializable {
	def this(vs: Double*) = this(vs.toArray[Double])
	val dimension: Int = items.length

	/**
	 * WARNING: best not to use this if possible
	 */
	def getArray: Array[Double] = items

	override def hashCode: Int = items.hashCode
	override def equals(other: Any): Boolean = {
		println("warning: you probably don't want to call equals on DVecs!")
		if(other.isInstanceOf[DVec]) {
			val o = other.asInstanceOf[DVec]
			if(dimension != o.dimension) return false
			for(i <- 0 until dimension)
				if(this(i) != o(i))
					return false
			return true
		}
		else false
	}

	def zeroOut { Arrays.fill(items, 0d) }
	def lInf: Double = items.map(math.abs).max
	def l2: Double = math.sqrt(items.map(x => x*x).sum)
	def l1: Double = items.map(math.abs).sum
	def apply(i: Int): Double = items(i)
	def ++(other: DVec) = VecOps.concat(this, other)
	override def iterator: Iterator[(Int, Double)] = (0 until dimension).iterator.map(i => (i, items(i)))
	override def toString: String = "dv[" + items.mkString(", ") + "]"
	def values = iterator.map(_._2)

	def linearTransform(scale: Double, shift: Double): DVec =
		new DVec(items.map(x => x*scale + shift))

	// holy moly this caused me some dumb bugs
	// it turns out that my brain always assumes that these methods
	// modify the state of the vector (rather than return a new one)
	// so I'm going to remove them for fear that they cause bugs rather than be helpful
	//def scale(factor: Double) = linearTransform(factor, 0d)

	def *=(factor: Double) {
		var i = 0
		while(i < dimension) {
			items(i) *= factor
			i += 1
		}
	}
	def *(factor: Double): DVec = {
		val c = this.copy
		c *= factor
		c
	}

	def +(other: DVec): DVec = {
		val c = this.copy
		c += other
		c
	}

	def -(other: DVec): DVec = {
		val c = this.copy
		VecOps.add(c, other, -1d)
		c
	}

	def +=(other: SVec) { VecOps.add(this, other, 1d) }
	def +=(other: DVec) { VecOps.add(this, other, 1d) }

	def update(idx: Int, value: Double) { items(idx) = value }
	def setBacking(buf: Array[Double]) { items = buf }
	def minEquals(other: DVec) {
		assert(dimension == other.dimension)
		var i = 0
		while(i < dimension) {
			val z = other(i)
			if(z < items(i))
				items(i) = z
			i += 1
		}
	}
	def maxEquals(other: DVec) {
		assert(dimension == other.dimension)
		var i = 0
		while(i < dimension) {
			val z = other(i)
			if(z > items(i))
				items(i) = z
			i += 1
		}
	}
	def copy: DVec = {
		val na = Array.ofDim[Double](items.length)
		Array.copy(items, 0, na, 0, items.length)
		new DVec(na)
	}
	def copyTo(buf: Array[Double]) {
		assert(buf.length == dimension)
		java.lang.System.arraycopy(items, 0, buf, 0, items.length)
	}

	/**
	 * for debugging
	 */
	def containsBadValues(checkForNaN: Boolean = true, checkForInf: Boolean = true): Boolean = {
		def bad(d: Double) = (checkForNaN && d.isNaN) || (checkForInf && d.isInfinite)
		val allGood = items.forall(d => !bad(d))
		!allGood
	}

}

object DVec {
	def zero(i: Int): DVec = new DVec(Array.ofDim[Double](i))
	val empty = zero(0)
	val zero1 = zero(1)
	val zero2 = zero(2)
	val zero3 = zero(3)
	val zero4 = zero(4)
	val zero5 = zero(5)
	def rep(value: Double, n: Int) = new DVec(Array.fill(n)(value))

	def apply(values: Double*): DVec = new DVec(values.toArray)

	def max(a: DVec, b: DVec): DVec = {
		if(a.dimension != b.dimension)
			throw new RuntimeException("must match in dimension: %d != %d".format(a.dimension, b.dimension))
		val n = a.dimension
		val c = Array.ofDim[Double](n)
		var i = 0
		while(i < n) {
			c(i) = if(a(i) > b(i)) a(i) else b(i)
			i += 1
		}
		new DVec(c)
	}

	def sum(a: DVec, b: DVec): DVec = {
		if(a.dimension != b.dimension)
			throw new RuntimeException("must match in dimension: %d != %d".format(a.dimension, b.dimension))
		val n = a.dimension
		val c = Array.ofDim[Double](n)
		var i = 0
		while(i < n) {
			c(i) = a(i) + b(i)
			i += 1
		}
		new DVec(c)
	}

	import util.Random
	def gaussian(dim: Int, stdDev: Double, rand: Random): DVec = {
		val d = DVec.zero(dim)
		for(i <- 0 until dim)
			d(i) = rand.nextGaussian() * stdDev
		d
	}
}

