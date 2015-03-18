// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

/**
 * 
 */
package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.feature_interfaces.AlignmentSimilarity
import edu.jhu.hlt.parma.types.Document
import edu.jhu.hlt.parma.types.SVec
import edu.jhu.hlt.parma.types.DocAlignment
import edu.jhu.hlt.parma.types.Alignment
import edu.jhu.hlt.parma.types.Token
import edu.jhu.hlt.parma.types.Sentence
import edu.jhu.hlt.parma.types.Dependency
import approxlib.tree.LblTree
import approxlib.distance.EditDist
import approxlib.distance.EditBasedDist
import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap

/**
 * @author Xuchen Yao
 * original code in ~xuchen/parma/src/main/scala/edu/jhu/parma/features/TEDAlignment.scala
 * @author Charley Beller, modified and updated, no longer depends on Stanford
 */
class TEDAlignment extends AlignmentSimilarity {

	val NumOfAlignInBothMention = "NumOfTEDAlignInBothMention"
	val NumOfAlignInReportMention = "NumOfTEDAlignInReportMention"
	val NumOfAlignInPassageMention = "NumOfTEDAlignInPassageMention"
	val HeadAlign = "HeadTEDAlignInMentions"

	@transient
	var pair2edit: HashMap[String, EditDist] = null

	override def setup(trainDocs: java.util.Collection[DocAlignment]): Unit = {	}

	override def cleanup(): Unit = { pair2edit.clear }

	private[this] val distBinarizer = new edu.jhu.hlt.parma.util.FixedWidthBinarizer(5, false, 0d, 5d)

	def makeTreeString(sentence: Sentence) = treeString(buildTree(sentence))

	def buildTree(sentence: Sentence): Map[String, Seq[String]] = {
		def escape(word: String): String = {
			word.replaceAll(":", "#colon#").replaceAll("/", "#slash#")
				.replaceAll("\\{", "#left_curly_brace#").replaceAll("\\}", "#right_curly_brace#")
		}
		def loop(deps: Seq[Dependency[Token]], tmap: Map[String, Seq[String]]): Map[String, Seq[String]] = {
			if (deps.isEmpty) tmap
			else {
				val d = deps.head
				val key = if (d.typ == "root") "-1:aRoot" else d.gov.index+":"+d.gov.getWord
				val word = escape(d.dep.getWord)
				val lemma = escape(d.dep.getLemma)
				val pos = escape(d.dep.getPosTag)
				val node = d.dep.index +":"+ word +"/"+ lemma +"/"+ pos +"/"+ d.typ
				val entry = tmap.getOrElse(key, Seq()) ++ Seq(node)
				val tmap1 = tmap + (key -> entry)
				loop(deps.tail, tmap1)
			}
		}
		loop(sentence.dependencies, Map.empty)
	}

	private def treeString(tree: Map[String, Seq[String]]): String = {
		def nodeString(tree: Map[String, Seq[String]], node: String, traversed: String): String = {
			//println("node: "+node)
			val ts = new collection.mutable.StringBuilder("{")
			ts.append(node)
			if (node.length > 0) {
				val word = if (node == "-1:aRoot") node else node.split("/")(0)
				//println("word: "+word)
				if (tree.keys.contains(word) && traversed.indexOf(word) < 0){ 
				  //println("entries: "+tree(word))
					tree(word).foreach {entry => ts.append(nodeString(tree, entry, ts.toString))}
				}
			}
			ts.append("}")
			ts.toString
		}
		"-1:" + nodeString(tree, "-1:aRoot", "") 
	}
    
	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {
		val (rcm, pcm) = edu.jhu.hlt.parma.inference.CanonicalMentionFinder.canonicalMentions(a, report, passage)

		val rs = report.getSentence(rcm)
		val ps = passage.getSentence(pcm)


		//println("rs: "+rs)
		//println(makeTreeString(rs))
		//println(makeTreeString(ps))
		val reportTree = makeTreeString(rs)
		//println("ps: "+ps)
		val passageTree = makeTreeString(ps)

		val reportLblTree = LblTree.fromString(reportTree)
		val passageLblTree = LblTree.fromString(passageTree)

		val pairId = report.id + rcm.getSentenceIdx.toString + "->" + 
			passage.id + pcm.getSentenceIdx.toString 

		val dist = {	
			if(pair2edit == null)
				pair2edit = new HashMap[String, EditDist]
			if (!pair2edit.contains(pairId)) { 
				val ed = new EditDist(true)
				ed.treeDist(reportLblTree, passageLblTree)
				ed.printHumaneEditScript()
				pair2edit.update(pairId, ed)
			}
			pair2edit(pairId)
		} 

		val tdistance = dist.treeDist(reportLblTree, passageLblTree)

		var headAlign = 0
		var alignInBoth = 0
		var alignInReport = 0
		var alignInPassage = 0
		
		var norm: Double = 0d
		dist.getAlignInWordOrder1to2().entrySet().foreach { entry =>
			val r = entry.getKey
			val p = entry.getValue
			if (r >= rcm.getStartTokenIdx && r < rcm.getEndTokenIdx &&
					p >= pcm.getStartTokenIdx && p < pcm.getEndTokenIdx) {
				alignInBoth += 1
				if (r == rcm.getHeadTokenIdx && p == pcm.getHeadTokenIdx) {
					headAlign = 1
				}
			}
			else {
				if (r >= rcm.getStartTokenIdx && r < rcm.getEndTokenIdx) {
					alignInReport += 1
				}
				if (p >= pcm.getStartTokenIdx && p < pcm. getEndTokenIdx) {
					alignInPassage += 1
				}
			}
			norm += 1d
		}

		b(sv, tdistance, "TEDdistance", distBinarizer)
		b(sv, headAlign / norm, "TEDheadAlign")
		b(sv, "TED#headAlign=" + headAlign)
		b(sv, alignInBoth / norm, "TEDbothAlign")
		b(sv, "TED#bothAlign=" + alignInBoth)
		b(sv, alignInReport / norm, "TEDreportAlign")
		b(sv, alignInPassage / norm, "TEDpassageAlign")
		b(sv, "TEDminRPalign=" + math.min(alignInReport, alignInPassage))
		b(sv, "TEDmaxRPalign=" + math.max(alignInReport, alignInPassage))
	}
}

/* vim: set noet : */
