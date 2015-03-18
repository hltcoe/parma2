// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util

import collection.JavaConversions._
import java.util.HashMap

class Bijection[A,B](
		protected val forwards: HashMap[A,B],
		protected val backwards: HashMap[B,A]) {
	def this() = this(new HashMap[A,B], new HashMap[B,A])
	def getOptionForwards(a: A): Option[B] = {
		val mb = forwards.get(a)
		if(mb == null) None
		else Some(mb)
	}
	def getForwards(a: A): B = getOptionForwards(a) match {
		case Some(b) => b
		case None => throw new RuntimeException("nothing matches %s (size=%d)".format(a, size))
	}
	def getOptionBackwards(b: B): Option[A] = {
		val ma = backwards.get(b)
		if(ma == null) None
		else Some(ma)
	}
	def getBackwards(b: B): A = getOptionBackwards(b) match {
		case Some(a) => a
		case None => throw new RuntimeException("nothing matches " + b)
	}
	def leftValues: Set[A] = forwards.keySet.toSet
	def rightValues: Set[B] = backwards.keySet.toSet
	def size = forwards.size

	def longString: String = {
		val sb = new StringBuilder
		for((k,v) <- forwards)
			sb.append("%s <=> %s\n".format(k,v))
		sb.toString
	}
}

/**
 * does not tolerate duplicate keys in either domain
 */
class MutableBijection[A,B]() extends Bijection[A,B] {

	def add(a: A,  b: B) {
		val oldB = forwards.put(a,b)
		if(oldB != null)
			throw new RuntimeException("a=%s is not uniq: b=%s oldB=%s".format(a, b, oldB))
		val oldA = backwards.put(b,a)
		if(oldA != null)
			throw new RuntimeException("b=%s is not uniq: a=%s oldA=%s".format(b, a, oldA))
	}

	/**
	 * does not allocate anything new
	 * i.e. only the type changes and you can still mutate the Bijection
	 * it is up to the caller of this function to either release the
	 * reference to the mutable version or never mutate it.
	 */
	def freeze: Bijection[A,B] = this.asInstanceOf[Bijection[A,B]]

	/**
	 * makes a copy of this
	 */
	def freezeSafe: Bijection[A,B] = {
		val f = new HashMap[A,B]
		val b = new HashMap[B,A]
		forwards.entrySet.foreach(x => {
			val b1 = f.put(x.getKey, x.getValue)
			val b2 = b.put(x.getValue, x.getKey)
			if(b1 != null || b2 != null)
				throw new IllegalStateException
		})
		new Bijection(f, b)
	}

	def clear {
		forwards.clear
		backwards.clear
	}
}

