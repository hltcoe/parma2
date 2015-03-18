// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util

import edu.jhu.hlt.parma.types._
import io.Source
import java.io.File
import collection.mutable.ArrayBuffer

object MentionFileUtil extends Logging2 {

	val sep = "\t"
	val headerStr = "mention-id" + sep + "(predicate|argument)" + sep + "doc-id" + sep + "sentence-idx" +
		sep + "start-token-id" + sep + "end-token-idx" + sep + "head-token-idx" + sep + "word" + sep + "# optional comments"

	case class MentionRef(val id: String, val kind: String, val docId: String,
			val sentIdx: Int, val startTokIdx: Int, val endTokIdx: Int, val headTokIdx: Int,
			val word: String, val comment: Option[String] = None) {

		def this(id: String, p: Predicate, doc: Document) =
			this(id, "predicate", doc.id, p.location.getSentenceIdx, p.location.getStartTokenIdx, p.location.getEndTokenIdx,
				p.location.getHeadTokenIdx, doc.getHeadToken(p).getWord, None)

		def this(id: String, a: Argument, doc: Document) =
			this(id, "argument", doc.id, a.location.getSentenceIdx, a.location.getStartTokenIdx, a.location.getEndTokenIdx,
				a.location.getHeadTokenIdx, doc.getHeadToken(a).getWord, None)

		if(!Set("predicate", "argument").contains(kind))
			throw new IllegalArgumentException("kind = " + kind)

		def toLine: String = id + sep + kind + sep + docId + sep + sentIdx +
			sep + startTokIdx + sep + endTokIdx + sep + headTokIdx + sep + word + (comment match {
				case Some(c) => sep + c
				case None => ""
			})
		def toMention: Mention = MentionBuilder.from(sentIdx, startTokIdx, endTokIdx, headTokIdx)
		def toPredArg: Either[Predicate, Argument] = kind match {
			case "predicate" => Left(Predicate(toMention))
			case "argument" => Right(Argument(toMention))
		}
		def isPredicate: Boolean = kind == "predicate"
		def isArgument: Boolean = kind == "argument"
	}

	import AlignmentFileUtil.AlignmentRef
	def readMentionsFrom(f: File, header: Boolean = false, onlyTakeMentionsFor: Option[Set[AlignmentRef]] = None): Seq[MentionRef] = {
		log("[MentionFileUtil] reading mentions from " + f.getPath)

		val goodMentionIds: Set[String] = onlyTakeMentionsFor match {
			case None => new Set[String] {
				override def contains(s: String): Boolean = true
				override def iterator: Iterator[String] = throw new UnsupportedOperationException
				override def +(e: String): Set[String] = throw new UnsupportedOperationException
				override def -(e: String): Set[String] = throw new UnsupportedOperationException
			}
			case Some(sar) => sar.flatMap(ar => Seq(ar.reportMentionId, ar.passageMentionId)).toSet[String]
		}

		val mfLine = """(\S+)\t(predicate|argument)\t(\S+)\t(\d+)\t(\d+)\t(\d+)\t(\d+)\t(\S+)(\s*#.*)?""".r
		Source.fromFile(f).getLines.drop(if(header) 1 else 0).flatMap(_ match {
			case mfLine(id, kind, docId, sentIdx, startTokIdx, endTokIdx, headTokIdx, word, comment) if goodMentionIds.contains(id) =>
				Some(MentionRef(id, kind, docId, sentIdx.toInt, startTokIdx.toInt, endTokIdx.toInt,
					headTokIdx.toInt, word, if(comment == null) None else Some(comment.trim())))
			case line => None
		}).toBuffer[MentionRef].toSeq
	}

	def writeMentionsTo(f: File, mentions: Seq[MentionRef], header: Boolean = false) {
		log("[MentionFileUtil] wriging %d mentions to %s...".format(mentions.size, f.getPath))
		val w = FileUtils.getWriter(f)
		if(header) w.write(headerStr + "\n")
		for(m <- mentions) w.write(m.toLine + "\n")
		w.close
	}

	/**
	 * input: a Concrete Communications file with Situatitons and Entities
	 * output: a mentions file
	 */
	def main(args: Array[String]) {
		if(args.length != 2) {
			println("please provide:")
			println("1) a Concrete Communications files where the Communications have Entities and Situations")
			println("2) a mentions file to dump the output into")
			return
		}
		val commFile = new File(args(0))
		val outFile = new File(args(1))
		var docs: Seq[Document] = null
		try {
			docs = ConcreteWrapper.getDocumentsFrom(commFile)
		} catch {
			case e: Exception =>
				e.printStackTrace
				System.err.flush
				println("\nreading the Communications crashed; are you SURE they have Entities and Situations in them?")
				return
		}
		val mentions = new ArrayBuffer[MentionRef]
		for(d <- docs) {
			for(p <- d.predicates) {
				val id = "%s-p%d.%d-%d".format(d.id, p.location.getSentenceIdx, p.location.getStartTokenIdx, p.location.getEndTokenIdx)
				mentions += new MentionRef(id, p, d)
			}
			for(a <- d.arguments) {
				val id = "%s-a%d.%d-%d".format(d.id, a.location.getSentenceIdx, a.location.getStartTokenIdx, a.location.getEndTokenIdx)
				mentions += new MentionRef(id, a, d)
			}
		}
		println("read %d preds/args from %d documents".format(mentions.size, docs.size))
		writeMentionsTo(outFile, mentions.toSeq)
	}
}

