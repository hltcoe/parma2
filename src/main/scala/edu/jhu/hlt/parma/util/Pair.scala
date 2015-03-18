// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util

object PairImplicits {
	implicit def convert[A, B](in: Pair[A])(implicit ev: A => B): Pair[B] = in.map(ev)
}

class Pair[T](val left: T, val right: T) {
  
	def this(p: (T, T)) = this(p._1, p._2)
	
	override def toString: String = "(Pair %s %s)".format(left, right)
	override def hashCode: Int = (left.hashCode << 16) | right.hashCode
	override def equals(other: Any): Boolean = {
		if(other.isInstanceOf[Pair[_]]) {
			val o = other.asInstanceOf[Pair[_]]
			left == o.left && right == o.right
		}
		else false
	}

	def map[S](f: T => S): Pair[S] = new Pair(f(left), f(right))
	
	def allPairs[S](f: T => Seq[S]): Seq[Pair[S]] = {
		for(l <- f(left); r <- f(right)) yield new Pair(l, r)
	}
	
}

