// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.types

trait HasLocation {
    def location: Mention
	override def equals(other: Any): Boolean = {
		if(other.isInstanceOf[HasLocation])
			location == other.asInstanceOf[HasLocation].location
		else false
	}
	override def hashCode: Int = location.hashCode
}

