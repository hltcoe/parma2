// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.types

case class Predicate(val location: Mention) extends HasLocation {

	override def toString: String = "(Predicate %s)".format(location)
	
	override def equals(other: Any): Boolean = {
		if(other.isInstanceOf[Predicate]) {
			val o = other.asInstanceOf[Predicate]
			location == o.location
		}
		else false
	}
	
	override def hashCode: Int = location.hashCode
}

