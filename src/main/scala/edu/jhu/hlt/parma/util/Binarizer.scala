// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util

/**
 * binarize may only return one integer (bucket index)
 * if you want more complicated bucketing schemes, then make
 * multiple binarizers (e.g. one coarse and one fine grain binarizer)
 */
trait Binarizer extends Serializable {
	/**
	 * if false, calls to binarize should be interpretted as
	 * just that one bucket as "firing"
	 * if true, then all buckets less than the index returned by
	 * calls to binarize
	 */
	val useBucketRanges: Boolean
	val numBuckets: Int
	def binarize(v: Double): Int
}

/**
 * has extra buckets for special values:
 * - for values below low
 * - [regular buckets, numBuckets of them]
 * - for values above high
 * - for NaN and Inf
 */
class FixedWidthBinarizer(val numRegularBuckets: Int, override val useBucketRanges: Boolean, val low: Double, val high: Double) extends Binarizer {
	val bucketWidth = (high - low) / numRegularBuckets
	override val numBuckets = numRegularBuckets + 2
	def binarize(v: Double): Int = {

		// 0 => special case
		if(java.lang.Double.isNaN(v) || java.lang.Double.isInfinite(v)) 0
		
		// 1 => below low
		else if(v < low + bucketWidth/2d) 1

		// 2 => regular range + above high
		else {
			val b = ((v - (low + bucketWidth/2d)) / bucketWidth).toInt + 2
			if(b >= numBuckets)
				numBuckets - 1
			else b
		}
	}

	/**
	 * use this value when you have nothing to say with this feature value
	 */
	val agnostic: Double = java.lang.Double.NaN
}

/**
 * Makes evenly sized buckets with highest bucket picking up overflow:
 * e.g. 0-10 in 3 buckets: 0-2 3-5 6-10
 */
class IntBucketBinarizer(val numRegularBuckets: Int, override val useBucketRanges: Boolean, val low: Int, val high: Int) extends Binarizer {
  val bucketWidth = (high - low) / numRegularBuckets
  override val numBuckets = numRegularBuckets 
    
  def binarize(v: Int): Int = {
    if (v < low) 0
    else {
      val b = ((v / bucketWidth) + 1)
      if (b >= numBuckets)
        numBuckets 
      else b
    }
  }
  
  def binarize(v: Double): Int = {
    binarize(v.toInt)
   }
}


