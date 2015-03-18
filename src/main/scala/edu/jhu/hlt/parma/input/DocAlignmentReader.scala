// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.input

import edu.jhu.hlt.parma.types._
import java.io.File

trait DocAlignmentReader[DA <: DocAlignment] {
	
	/**
	 * returns gold alignments
	 */
	def getDocAlignments: Seq[DA]
	
	/**
	 * give the domain string, used for domain adaptation
	 */
	def domain: String = this.getClass.getName.replaceAll("edu.jhu.hlt.parma.input.", "")
	
	/**
	 * returns a (uniq) seq/set of documents, taken from the
	 * getDocAlignments method
	 */
	def getDocuments: Seq[Document] =
		getDocAlignments.flatMap(da => List(da.report, da.passage)).toSet.toIndexedSeq
}

