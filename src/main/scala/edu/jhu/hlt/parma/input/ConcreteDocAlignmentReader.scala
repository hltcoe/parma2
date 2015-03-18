// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.input

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import java.io.{ File, BufferedWriter }

// TODO i could expose this as extending DocAlignmentReader[ParametricDocAlignment[CommunicationDocument]]
// but this is a slipery slope that might lead to over dependence on Concrete...

class ConcreteDocAlignmentReader(var file: File, domainStr: Option[String] = None) extends DocAlignmentReader[DocAlignment] {
	def this() = this(ParmaConfig.getFile("data.concrete.default.alignments"))
	override val domain = domainStr match {
		case Some(s) => s
		case None => file.getPath
	}
	override def getDocAlignments: Seq[DocAlignment] = {
		println("[ConcreteDocAlignmentReader] reading from " + file.getPath)

		// this is 2x slower than the following method
		//daIter.toBuffer.toSeq

		import collection.mutable.ArrayBuffer
		val buf = new ArrayBuffer[DocAlignment]
		val iter = daIter
		while(iter.hasNext)
			buf += iter.next
		buf.toSeq
	}

	def daIter: Iterator[DocAlignment] = {
		val iter = new ConcreteDocAlignmentIterator(file)
		domainStr match {
			case None => iter
			case Some(str) =>
				// replace the original domain, which will always be "Annotated Gigaword"
				iter.map(da =>
					new DocAlignment(da.id, Some(str), da.report, da.passage,
						da.sureAlignments, da.exactlyPossibleAlignments)
				)
		}
	}
}

object ConcreteDocAlignmentReader {

	def RF: ConcreteDocAlignmentReader =
		new ConcreteDocAlignmentReader(ParmaConfig.getFile("data.concrete.rf.alignments"), Some("RF"))
	
	def EECB: ConcreteDocAlignmentReader =
		new ConcreteDocAlignmentReader(ParmaConfig.getFile("data.concrete.eecb.alignments"), Some("EECB"))
	
	def MTC: ConcreteDocAlignmentReader =
		new ConcreteDocAlignmentReader(ParmaConfig.getFile("data.concrete.mtc.alignments"), Some("MTC"))
	
	def GV: ConcreteDocAlignmentReader =
		new ConcreteDocAlignmentReader(ParmaConfig.getFile("data.concrete.gv.alignments"), Some("GV"))

	def getDocAlignments(f: File): Seq[DocAlignment] = {
		new ConcreteDocAlignmentReader(f).getDocAlignments
	}

	/**
	 * print out DocAlignments to inspect them
	 */
	def main(args: Array[String]) {
		if(args.length < 1 || args.length > 2) {
			println("purpose: print out a human readable form for a set of alignments")
			println("please provide:")
			println("1) an alignments file (Concrete protobufs)")
			println("2) [optional] a file to print the alignments (default is to print to stdout)")
			return
		}
		val w: BufferedWriter =
			if(args.length == 2) FileUtils.getWriter(new File(args(1)))
			else null
		for(da <- ConcreteDocAlignmentUtils.deserialize(new File(args(0)))) {
			val str = ("=" * 100) + "\n" +
				Describe.docAlignment(da) +
				"Report = " + Describe.document(da.report) + da.report.sentences.mkString("\n") + "\n" +
				"Passage = " + Describe.document(da.passage) + da.passage.sentences.mkString("\n") + "\n" +
				"\n"
			if(w != null) w.write(str)
			else println(str)
		}
		if(w != null) w.close
	}
}

