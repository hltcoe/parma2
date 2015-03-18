// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.types

import scala.collection.mutable.ArrayBuffer

case class ArgumentCoref(val chain: ArrayBuffer[Argument]) extends Iterable[Argument] {

	def this(c: Seq[Argument]) = this(new ArrayBuffer ++= c)
	def this(a: Argument) = this(Seq(a))
	def this() = this(Seq())

	override def size = chain.size

	override def iterator = chain.iterator

	override def equals(other: Any): Boolean = {
		if(other.isInstanceOf[ArgumentCoref]) {
			val o = other.asInstanceOf[ArgumentCoref]
			size == o.size && chain.zip(o.chain).forall(aa => aa._1 == aa._2)
		}
		else false
	}

	override def hashCode: Int = {
		var h = 0
		chain.foreach(a => h = h*877 + a.hashCode)	// yes i know i could use reduce...
		h
	}
}

