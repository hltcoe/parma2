// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.types

case class Context(val report: Document, val passage: Document) {
	override val hashCode: Int = (report.hashCode << 16) | passage.hashCode
	override def equals(other: Any): Boolean = {
		if(other.isInstanceOf[Context]) {
			val o = other.asInstanceOf[Context]
			report == o.report && passage == o.report
		}
		else false
	}
}


