// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.annotation

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.input._
import edu.jhu.hlt.parma.util._
import scala.collection.JavaConversions._
import java.io.File

/**
 * dump mentions and alignments from DocAlignments into
 * mentions files and alignments file (see google doc on file formats)
 */
object MTurkUtils {
  
  	// produces mention ids for predicates
	private def predicateIdsFor(d: Document): Map[Predicate, String] = {
		val m = d.predicates.map(pred => {
			val l = pred.location
			(pred, "%s-p%d.%d".format(d.id, l.getSentenceIdx, l.getHeadTokenIdx))
		}).toMap
		assert(m.size == d.predicates.size)
		m
	}
	
  	// produces mention ids for arguments
	private def argumentIdsFor(d: Document): Map[Argument, String] = {
		val m = d.arguments.map(arg => {
			val l = arg.location
			(arg, "%s-a%d.%d".format(d.id, l.getSentenceIdx, l.getHeadTokenIdx))
		}).toMap
		assert(m.size == d.arguments.size)
		m
	}
  
	private def mentionsIn(d: Document): Seq[String] = {
		val format = (id: String, kind: String, m: Mention) => {
			List(id, kind, d.id, m.getSentenceIdx, m.getStartTokenIdx,
				m.getEndTokenIdx, m.getHeadTokenIdx, d.getHeadString(m)).mkString("\t")
		}
		val pmap = predicateIdsFor(d)
		val pStrs = d.predicates.map(pred => {
			format(pmap(pred), "predicate", pred.location)
		})
		val amap = argumentIdsFor(d)
		val aStrs = d.arguments.map(arg => {
			format(amap(arg), "argument", arg.location)
		})
		pStrs ++ aStrs
	}
  
	def dumpMentions(f: File, docs: Seq[Document], includeHeader: Boolean = true) {
		println("dumping mentions to " + f.getCanonicalPath + "...")
		val bw = FileUtils.getWriter(f)
		if(includeHeader)
			bw.write("# mentionId\t(predicate|argument)\tdocId\tsentenceIdx\tstartIdx\tendIdx\theadIdx\theadWord")
		bw.newLine
		docs.foreach(d => {
			mentionsIn(d).foreach(s => {
				bw.write(s)
				bw.newLine
			})
		})
		bw.close
	}

	/**
	 * docAlignment.id can be used for other things and hence, alignments,
	 * which would otherwise match, have different ids
	 * e.g. "AA_r14.3_p14.4" vs "r14.3_p14.4"
	 */
	def stableDocAlignmentId(da: DocAlignment): String = "r%s_p%s".format(da.report.id, da.passage.id)
	
	private def alignmentsIn(da: DocAlignment): Seq[String] = {
	  
		val prIds = predicateIdsFor(da.report)
		val ppIds = predicateIdsFor(da.passage)
		val arIds = argumentIdsFor(da.report)
		val apIds = argumentIdsFor(da.passage)
		
		val a2s = (a: Alignment) => a match {
			case pa: PredicateAlignment => {
				//val rs = List(prIds(pa.reportPred), da.report.id, stableDocAlignmentId(da), pa.reportPred.getCorefSet, "sure").mkString("\t")
				//val ps = List(ppIds(pa.passagePred), da.passage.id, stableDocAlignmentId(da), pa.passagePred.getCorefSet, "sure").mkString("\t")
				//List(rs, ps)
				List(List(stableDocAlignmentId(da), prIds(pa.reportPred), ppIds(pa.passagePred), "sure").mkString("\t"))
			}
			case aa: ArgCorefAlignment => {
				//val r = aa.reportCoref.map(a => List(arIds(a), da.report.id, stableDocAlignmentId(da), a.getCorefSet, "sure").mkString("\t"))
				//val p = aa.passageCoref.map(a => List(apIds(a), da.passage.id, stableDocAlignmentId(da), a.getCorefSet, "sure").mkString("\t"))
				//(r ++ p).toList
				aa.reportCoref.flatMap(ra => {
					aa.passageCoref.map(pa => {
						List(stableDocAlignmentId(da), arIds(ra), apIds(pa), "sure").mkString("\t")
					})
				})
			}
		}
		
		da.possibleAlignments.toSeq.flatMap(a2s)
	}
	
	def dumpAlignments(f: File, docAlignments: Seq[DocAlignment], includeHeader: Boolean = true) {
		println("dumping alignments to " + f.getCanonicalPath + "...")
		val bw = FileUtils.getWriter(f)
		if(includeHeader)
			bw.write("# assignmentId\tmentionId1\tmentionId2\t(sure|possible)")
		bw.newLine
		docAlignments.foreach(da => {
			alignmentsIn(da).foreach(s => {
				bw.write(s)
				bw.newLine
			})
		})
		bw.close
	}
	
	// tries to find a file to dump to in parma.config
	def dumpAlignments(docAlignments: Seq[DocAlignment], description: String) {
		val k = "diagnostics.alignments.mturk"
		val dirName = ParmaConfig.getDirectory(k, null)
		if(dirName == null) {
			println("[MTurkUtil dumpAlignments] not dumping mturk alignment file because %s is not set in parma.config".format(k))
			return
		}
		dumpAlignments(new File(dirName, description + ".txt"), docAlignments)
	}
	
	def main(args: Array[String]) {
		if(args.length != 2) {
			println("please provide:")
			println("1) a protobuf file containing documents and annotations (often via anno pipeline + concrete-agiga)")
			println("2) an output mention file")
			println("3) optional: a parma.config file")
			return
		}
		if(args.length == 2) ParmaConfig.load("parma.config")
		else ParmaConfig.load(args(2))
		
		val docs = ConcreteWrapper.getCommunicationsFrom(new File(args(0)))
			.map(new RichConcreteDocBuilder(_))
			.map(doc => PredArgSelector.identifyPredicatesAndArguments(doc)).toSeq
		val mentionFile = new File(args(1))
		
		dumpMentions(mentionFile, docs)
	}

	def otherMain(args: Array[String]) {
		if(args.length < 2 || args.length > 3) {
			println("please provide:")
			println("1) a mentions file")
			println("2) an alignments file")
			println("3) optional: a parma.config file")
			return
		}
		val mentionFile = new File(args(0))
		val alignmentFile = new File(args(1))

		println("args = " + args)
		println("args.length = " + args.length)
		if(args.length == 2) ParmaConfig.load("parma.config")
		else ParmaConfig.load(args(2))
		
		println("loading data...")
		val dr = ConcreteDocAlignmentReader.RF
		val alignments = dr.getDocAlignments.toBuffer
		val docs = alignments.flatMap(da => List(da.report, da.passage))
		
		dumpMentions(mentionFile, docs)
		dumpAlignments(alignmentFile, alignments)
	}

}


