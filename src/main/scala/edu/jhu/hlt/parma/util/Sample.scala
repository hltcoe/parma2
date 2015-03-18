package edu.jhu.hlt.parma.util

import collection.mutable.ArrayBuffer

/**
 * represents a sample of numbers, lets you compute
 * things like means and variances
 */
class Sample {

	// TODO replace with Array[Double] or equivalent
	// TODO reservoir sample
	// TODO only keep sufficient stats
	private val items = new ArrayBuffer[Double]

	def +=[T <: Double](item: T) { add(item) }
	def add[T <: Double](item: T) {
		require(!item.isInfinite)
		require(!item.isNaN)
		items += item
	}

	def size: Int = items.size

	def mean: Double = items.sum / items.size

	def meanCI(z: Double = 1.96d): (Double, Double) = {
		val err = z * stdDev
		val mu = mean
		(mu - err, mu + err)
	}

	def stdDev: Double = math.sqrt(variance)
	
	def variance: Double = {
		require(size > 1)
		val exx = items.map(x => x*x).sum / items.size
		val ex = mean
		exx - (ex * ex)
	}
	
	def sampleVariance: Double = {
		require(size > 1)
		val n = size
		variance * n / (n-1d)
	}

	def min: Double = items.min
	def max: Double = items.max
}

