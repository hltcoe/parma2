// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.types

class DocAlignmentWithFeatures[+F](val alignment: DocAlignment, val features: F) {
	def report = alignment.report
	def passage = alignment.passage
}

