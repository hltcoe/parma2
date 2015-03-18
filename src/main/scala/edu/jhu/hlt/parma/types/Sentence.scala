// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.types

import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.concrete.Concrete.{ TokenTagging }
import collection.mutable.{ ArrayBuffer, Buffer }
import collection.JavaConversions._

class RichConcreteSent(val concreteSent: edu.jhu.hlt.concrete.Concrete.Sentence, override val index: Int) extends Sentence {

	override val tokens: IndexedSeq[Token] = {
		val tlist = concreteSent.getTokenizationList
		assert(tlist.size == 1)
		val cTokenization = tlist.head
		assert(cTokenization.getKind == edu.jhu.hlt.concrete.Concrete.Tokenization.Kind.TOKEN_LIST)
		val cTokens = cTokenization.getTokenList
		def tagHelper(l: java.util.List[TokenTagging], name: String): Buffer[String] = {
			if(l.size == 0) {
				println("there are no %s tag lists, inserting \"???\"s".format(name))
				cTokens.map(t => "???")
			} else {
				if(l.size > 1) println("more than one %s tag list, taking the first".format(name))

				// check that indices match up
				val tt = l.head.getTaggedTokenList
				assert(tt.size == cTokens.size && tt.zip(cTokens).forall(tc => tc._1.getTokenIndex == tc._2.getTokenIndex),
					"indices don't match up:\n%s tags = %s\ntokens = %s".format(name, tt.mkString(", "), cTokens.mkString(", ")))
				
				tt.map(_.getTag)
			}
		}
		val posTT = tagHelper(cTokenization.getPosTagsList, "POS")
		val nerTT = tagHelper(cTokenization.getNerTagsList, "NER")
		val lemmaTT = tagHelper(cTokenization.getLemmasList, "lemma")
		//val posTT = cTokenization.getPosTagsList.head.getTaggedTokenList; assert(cTokenization.getPosTagsList.size == 1)
		//val nerTT = cTokenization.getNerTagsList.head.getTaggedTokenList; assert(cTokenization.getNerTagsList.size == 1)
		//val lemmaTT = cTokenization.getLemmasList.head.getTaggedTokenList; assert(cTokenization.getLemmasList.size == 1)

		val toks = new ArrayBuffer[Token]
		for((ctok, idx) <- cTokens.zipWithIndex)
			toks += new ParmaToken(idx, ctok.getText, lemmaTT(idx), posTT(idx), nerTT(idx), null)
		toks.toIndexedSeq
	}

	override lazy val dependencies: Seq[Dependency[Token]] = {
		val toks = concreteSent.getTokenizationList
		if(toks.size == 0)
			throw new RuntimeException("how is there no tokenization?")
		if(toks.size > 1)
			println("more than one tokenization, using the first")
		val depParseList = toks(0).getDependencyParseList
		val depP = depParseList.sortBy(dp => {
			val t = dp.getMetadata.getTool.toLowerCase
			if(t.contains("col-ccproc")) 1d
			else if(t.contains("col-deps")) 0.5d
			else 0d	// add more magic strings with preference scores
		})
		if(depP.size == 0) {
			println("there are no dependency parses!")
			Seq()
		}
		else {
			if(depP.filter(_.getMetadata.getTool.toLowerCase.contains("col-ccproc")).size > 1)
				println("there are more than one col-ccproc dependency parse, choosing: " + depP(0).getMetadata.getTool)
			val ds = new ArrayBuffer[Dependency[Token]]
			for(dep <- depP(0).getDependencyList) {
				val g = tokens(dep.getGov)
				val d = tokens(dep.getDep)
				ds += new Dependency[Token](dep.getEdgeType, g, d)
			}
			ds.toSeq
		}
	}

	override val ptbParse: Option[Tree] = {
		val toks = concreteSent.getTokenizationList
		assert(toks.size == 1)
		val parses = toks(0).getParseList
		assert(parses.size == 1)
		Some(TreeBuilder.from(parses.head.getRoot, this))
	}
}


/**
 * write all of your code to use this type, not with a more specific types
 */
trait Sentence {
	def index: Int
	def tokens: IndexedSeq[Token]
	def dependencies: Seq[Dependency[Token]]
	def ptbParse: Option[Tree]

	def size = tokens.size

	def apply(idx: Int) = tokens(idx)

	def apply(m: Mention) = tokens.slice(m.getStartTokenIdx, m.getEndTokenIdx)

	def before(m: Mention): IndexedSeq[Token] =
		tokens.filter(_.index < m.getStartTokenIdx)

	def after(m: Mention): IndexedSeq[Token] =
		tokens.filter(_.index >= m.getEndTokenIdx)

	def governs(m: Mention): Seq[Dependency[Token]] =
		dependencies.filter(d => m.contains(d.dep))

	def governedBy(m: Mention): Seq[Dependency[Token]] =
		dependencies.filter(d => m.contains(d.gov))

	def rawString: String = tokens.map(_.getWord).mkString(" ")

	def isDependentOfVerb(t: Token): Boolean =
		dependencies.filter(d => d.dep == t && d.gov.getPosTag.startsWith("V")).size > 0

	override def hashCode: Int = (index << 16) ^ tokens.size
	override def equals(other: Any): Boolean = {
		if(other.isInstanceOf[Sentence]) {
			val o = other.asInstanceOf[Sentence]
			index == o.index //&&
				tokens == o.tokens
			// TODO may need to add more if we need very fine-grained equals
		}
		else false
	}
	override def toString: String = tokens.map(_.getWord).mkString(" ")
}


