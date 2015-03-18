// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util

import edu.jhu.hlt.parma.types._
import scala.collection.mutable.{ Buffer, ArrayBuffer, HashMap }
import scala.collection.JavaConversions._

object Misc {
	
	def round(v: Double, places: Int) = "%%.%df".format(places).format(v).toDouble
	
	def eitherConcat[P,Q](p: Traversable[P], q: Traversable[Q]): Traversable[Either[P,Q]] =
		p.map(Left(_)) ++ q.map(Right(_))
	
	/**
	 * NOTE: can return NaN if a and b are size 0
	 */
	def dice[T](a: Set[T], b: Set[T]): Double = {
		if(a.size + b.size == 0)
			println("returning NaN from Misc.dice")
		val num = 2d * (a & b).size.toDouble
		num / (a.size + b.size)
	}

	def safeDice[T](a: Set[T], b: Set[T]): Double = {
		if(a.size + b.size == 0) 0d
		else dice(a, b)
	}
	
	def toCounts[T](items: Traversable[T]): Map[T, Double] = {
		items.groupBy(t => t).mapValues(_.size.toDouble)
	}
	
	def cosine[T](a: Map[T, Double], b: Map[T, Double]): Double = {
		if(a.size == 0 || b.size == 0) {
			println("WARNING: cosine with an empty vector!")
			0d
		} else {
			var dotP = 0d
			var aNorm = 0d
			for ((k, av) <- a) {
				aNorm += av * av
				dotP += av * b.getOrElse(k, 0d)
			}
			val bNorm = b.values.map(v => v * v).sum
			dotP / (math.sqrt(aNorm) * math.sqrt(bNorm))
		}
	}
	
	/**
	 * NOTE: can return NaN if a and b are size 0
	 */
	def jaccard[T](a: Set[T], b: Set[T]): Double = {
		1 - dice(a, b)
	}

	def safeJaccard[T](a: Set[T], b: Set[T]): Double = {
		if(a.size + b.size == 0) 1d
		else jaccard(a, b)
	}
	
	/**
	 * joining on a key K, find all pairs in matching values
	 * i.e. in (K -> V1) and (K -> V2), return (V1 X V2)
	 */
	def flatJoin[K, V](
			a: scala.collection.Map[K,Traversable[V]],
			b: scala.collection.Map[K,Traversable[V]]): Buffer[(V,V)] = {
		var alignments = new ArrayBuffer[(V,V)]
		for((k,vs_1) <- a) {
			val vs_2 = b.getOrElse(k, Seq())
			for(v1 <- vs_1; v2 <- vs_2)
				alignments += ((v1,v2))
		}
		alignments
	}

	/**
	 * joining on a key K, find all pairs in matching values
	 * i.e. in (K -> V1) and (K -> V2), return (V1 X V2)
	 */
	def join[K, V](
			a: scala.collection.Map[K,Traversable[V]],
			b: scala.collection.Map[K,Traversable[V]]):
			scala.collection.mutable.Map[K, Buffer[(V,V)]] = {
		
		val map = new HashMap[K, Buffer[(V,V)]]
		
		for((k,vs_1) <- a) {
			var alignments = new ArrayBuffer[(V,V)]
			val vs_2 = b.getOrElse(k, Seq())
			for(v1 <- vs_1; v2 <- vs_2)
				alignments += ((v1,v2))
			map += (k -> alignments)
		}
		map
	}
	
}
