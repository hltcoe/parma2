// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util

import io.Source
import java.io.File

/**
 * reportId and passageId must match Communication.getGuid.getCommunicationId
 * hitId is used to describe a doc pair (in principle there is nothing wrong with
 * having more than one hitId for a pair of documents, for e.g. redundant annotations)
 */
case class DocIdPair(val hitId: String, val reportId: String, val passageId: String)

/**
 * read a bunch of doc id pairs from a TSV
 */
object DocIdPairUtil extends Logging2 {
	def readDocIdPairs(f: File): Seq[DocIdPair] = {
		val docPairLine = """(\S+)\t(\S+)\t(\S+)""".r
		Source.fromFile(f).getLines.flatMap(_ match {
			case docPairLine(a, r, p) => Some(DocIdPair(a, r, p))
			case line =>
				warn("couldn't parse line: " + line)
				None
		}).toSeq
	}
}

