// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features.generic

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util.Pair

class Pipe[D, T](val name: String, val func: D => T) extends Serializable {

	// TODO is really only relevant for Accumulators/Similarity, but i'm hacking right now...
	def featureName(localIdx: Int): String = {
		//"%s[%d]".format(name, localIdx)
		name + "[" + localIdx + "]"
	}

	var debug = false	// used for printing details on how the function is computed

	// D => T => PT => R
	def bind[PT, R](p: Pipe[PT, R])(implicit ev: T => PT): Pipe[D, R] =
		new Pipe(name + "-" + p.name, (d: D) => p(ev(this.apply(d))))
	
	def debugBind[PT, R](p: Pipe[PT, R])(implicit ev: T => PT): Pipe[D, R] = {
		val newName = name + "-" + p.name
		val newFunc = (d: D) => {
			if(debug) println("[pipe debug] about to call: " + this.name)
			val inner = this.apply(d)
			val mid = ev(inner)
			if(debug) println("[pipe debug] about to call: " + p.name)
			val out = p.apply(mid)
			if(debug) {
				println("[Pipe debug] before = " + inner)
				//println("[Pipe debug] mid = " + mid)
				println("[Pipe debug] after = " + out)
			}
			if(p.isInstanceOf[Similarity[_]]) {
				assert(out.isInstanceOf[Seq[_]], "out=" + out)
				val sim = p.asInstanceOf[Similarity[_]]
				for(dv <- out.asInstanceOf[Seq[DVec]])
					assert(dv.dimension == sim.outputDimension, "out=%s p=%s".format(out, p))
			}
			if(this.isInstanceOf[Similarity[_]]) {
				assert(p.isInstanceOf[Accumulator])
				// TODO check ident-maybe-empty for dim of mid matching dimension
			}
			out
		}
		new Pipe(newName, newFunc)
	}
	
	def >>=[PT, R](p: Pipe[PT, R])(implicit ev: T => PT): Pipe[D, R] = debugBind(p)(ev)
	
	def apply(d: D): T = func(d)
	
	def withCaching: CachingPipe[D, T] = new CachingPipe(name, func)
}

class CachingPipe[A,B](name: String, func: A => B) extends Pipe(name, func) {
	private val cache = new java.util.HashMap[A, B]	// i hear java hashmaps are faster!
	private var (hits, misses) = (0, 0)
	override def apply(a: A) = {
		val b = cache.get(a)
		if(b == null) {
			misses += 1
			val bb = func(a)
			cache.put(a, bb)
			bb
		}
		else {
			hits += 1
			b
		}
	}
	override def toString = "(CachingPipe:%s %d memos, %d hits, %d misses)".format(name, cache.size, hits, misses)
	def clear = {
		hits = 0
		misses = 0
		cache.clear
	}
}

object GeneralPipeImplementations {
	def allPairs[T] = new Pipe[(Context, Seq[Pair[Set[T]]]), (Context, Seq[Pair[T]])]("allPairs", (t: (Context, Seq[Pair[Set[T]]])) => {
		val (c, spss) = t
		val pairs: Seq[Pair[T]] = spss.flatMap(pss => {
			for(ls <- pss.left; rs <- pss.right) yield new Pair(ls, rs)
		})
		pairs.foreach(p => {
			assert(p.left != null)
			assert(p.right != null)
		})
		(c, pairs)
	})
}


// TODO add a penalty in a Pipe used to shrink features towards 0

// two options (A is probably better):
// A) put a penalty on a transform
// B) let transforms put different penalties on each term in the Seq it returns

/*
 * double values returned by func are to be considered non-negative penalties
 * when a features has been fully bound together, it will be scaled by
 * 1/(1+penalty) so as to shrink it towards 0
 * 
 * if you have a penalty of infinity, simply do not include it in the output of func
 * 
 * penalty is sort of like a prior, but not quite a log-probability
 * it is more forgiving than multiplying by exp(-penalty)
 */

