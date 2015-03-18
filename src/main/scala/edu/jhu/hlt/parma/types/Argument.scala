// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.types

// TODO figure out how to store the coref set
// i don't actually think you need to do that
// corefSet is sort of an intermediate representation
// when building doc alignments...
// they should have been removed from the beginning
case class Argument(val location: Mention) extends HasLocation {

	override def toString: String = "(Argument %s)".format(location)
	
	override def equals(other: Any): Boolean = {
		if(other.isInstanceOf[Argument]) {
			val o = other.asInstanceOf[Argument]
			location == o.location
		}
		else false
	}
	
	override def hashCode: Int = location.hashCode
}
