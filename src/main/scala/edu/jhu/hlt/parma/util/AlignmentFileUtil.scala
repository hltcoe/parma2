// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util

import io.Source
import java.io.File

object AlignmentFileUtil extends Logging2 {

	val sep = "\t"
	val verbose = true
	val headerStr = "hit-id" + sep + "report-mention-id" + sep + "passage-mention-id" + sep + "(sure|possible|not_aligned)" + sep + "# optional comments"

	case class AlignmentRef(val hitId: String, val reportMentionId: String, val passageMentionId: String, val conf: String, val comment: Option[String] = None) {
		if(!Set("sure", "possible", "not_aligned").contains(conf))
			throw new IllegalArgumentException("conf = " + conf)
		def isSure: Boolean = conf == "sure"
		def isPossible: Boolean = conf == "possible"
		def isNotAligned: Boolean = conf == "not_aligned"
		def toLine: String = hitId + sep + reportMentionId + sep + passageMentionId + sep + conf + (comment match {
			case Some(c) => sep + c
			case None => ""
		})
	}

	def readAlignmentsFrom(f: File, header: Boolean = false): Seq[AlignmentRef] = {
		log("[AlignmentFileUtil] reading alignments from %s...".format(f.getPath))
		val aLine = """(\S+)\t(\S+)\t(\S+)\t(sure|possible|not_aligned)(\s*#.*)?""".r
		Source.fromFile(f).getLines.drop(if(header) 1 else 0).flatMap(_ match {
			case aLine(h, r, p, c, com) =>
				Some(AlignmentRef(h, r, p, c, if(com == null) None else Some(com.trim())))
			case line =>
				if(verbose) log("could not parse line: " + line)
				None
		}).toSeq
	}

	def writeAlignmentsTo(f: File, alignments: Seq[AlignmentRef], header: Boolean = false) {
		log("[AlignmentFileUtil] writing %d alignments to %s...".format(alignments.size, f.getPath))
		val w = FileUtils.getWriter(f)
		if(header) w.write(headerStr + "\n")
		alignments.foreach(a => w.write(a.toLine + "\n"))
		w.close
	}
}

