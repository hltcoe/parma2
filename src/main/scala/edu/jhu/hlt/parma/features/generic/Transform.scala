// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features.generic

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util.Pair

class Transform[A,B](name: String, val transform: (Document, A) => Seq[B])
	extends Pipe[(Context, Seq[Pair[A]]), (Context, Seq[Pair[B]])](name, (t: (Context, Seq[Pair[A]])) => {
		val (c, spa) = t
		val spb = spa.flatMap(pa => {
			val left = transform(c.report, pa.left)
			val right = transform(c.passage, pa.right)
			// both transforms must produce something (and presumably the same thing)
			if(left.size == 0 || right.size == 0)
				Seq()
			else if(left.size == right.size)
				left.zip(right).map(new Pair(_))
			else throw new RuntimeException
		})
		(c, spb)
	}) {
	
	def forSets: Transform[Set[A], Set[B]] = {
		val newName = name + "$sets"
		val newFunc = (d: Document, sa: Set[A]) => Seq(sa.flatMap(a => transform(d, a)))
		new Transform(newName, newFunc)
	}
	
	def forSeqs: Transform[Seq[A], Seq[B]] = {
		val newName = name + "$seqs"
		val newFunc = (d: Document, sa: Seq[A]) => {
			val out = sa.map(a => transform(d, a).head)
			//println("[in forSeqs] sa=" + sa + ", out=" + out)
			Seq(out)
		}
		new Transform(newName, newFunc)
	}
}

// TODO may want to pull out "filter" like transforms
// into a separate file/class
object TransformImplementation {

	implicit def addSeq[T](t: T): Seq[T] = Seq(t)	// ONLY USE THIS HERE!
	
	val dependentTokens = new Transform[Mention, Set[Token]]("dependentTokens", (d: Document, m: Mention) => {
		Seq(d.governedBy(m).map(_.dep).toSet)
	})
	
	val governingTokens = new Transform[Mention, Set[Token]]("governingTokens", (d: Document, m: Mention) => {
		Seq(d.governs(m).map(_.gov).toSet)
	})

	val headToken = new Transform[Mention, Token]("headToken", (d: Document, m: Mention) => {
		val t = d.getHeadToken(m)
		assert(t != null)
		t
	})
	
	val allTokens = new Transform[Mention, Set[Token]]("allTokens", (d: Document, m: Mention) => {
		Seq(d.getMentionTokens(m).toSet)
	})
	
	val fullMentionString = new Transform[Mention, String]("fullMentionString", (d: Document, m: Mention) => {
		d.getMentionString(m)
	})
	
	val beforeHeadString = new Transform[Mention, String]("beforeHeadString", (d: Document, m: Mention) => {
		if(m.getStartTokenIdx == m.getHeadTokenIdx) Seq()
		else Seq(d.getSentence(m).before(m).map(_.getWord).mkString(" "))
	})
	
	val afterHeadString = new Transform[Mention, String]("afterHeadString", (d: Document, m: Mention) => {
		if(m.getHeadTokenIdx+1 == m.getEndTokenIdx) Seq()
		else Seq(d.getSentence(m).after(m).map(_.getWord).mkString(" "))
	})

	val lowercase = new Transform[String, String]("LC", (d: Document, s: String) => s.toLowerCase)
	
	def prefix(i: Int) = new Transform[String, String]("prefix"+i, (d: Document, s: String) => {
		assert(i > 0)
		if(s.size >= i) Seq(s.take(i))
		else Seq()
	})
	
	def suffix(i: Int) = new Transform[String, String]("suffix"+i, (d: Document, s: String) => {
		assert(i > 0)
		if(s.size >= i) Seq(s.takeRight(i))
		else Seq()
	})
	
	def ngrams(i: Int) = new Transform[String, Set[String]](i+"grams", (d: Document, s: String) => {
		assert(i > 0)
		val padding = "#"*(i-1)
		Seq((padding + s + padding).sliding(i).toSet)
	})
	
	val ner = new Transform[Token, String]("ner", (d: Document, t: Token) => {
		if(t.getNerTag == null) "???"
		else t.getNerTag
	})
	
	val normNer = new Transform[Token, String]("normNer", (d: Document, t: Token) => {
		if(t.getNormNer == null) "???"
		else t.getNormNer
	})
	
	val pos = new Transform[Token, String]("pos", (d: Document, t: Token) => {
		if(t.getPosTag == null) "???"
		else t.getPosTag
	})
	
	val lemma = new Transform[Token, String]("lemma", (d: Document, t: Token) => {
		assert(t.getLemma != null)
		t.getLemma
	})
	
	val word = new Transform[Token, String]("word", (d: Document, t: Token) => {
		assert(t.getWord != null)
		t.getWord
	})
	
	/** TODO right now this violates the contract used in FeatureIndexer.addStable because
	 * it can return an empty Seq (which is later translated into an empty
	 * DVec, which violates the principle that all features must return a
	 * fixed-dimension value)
	 * DO NOT USE THIS UNTIL YOU ACCOUNT FOR THIS
	 * this could be accomplished by putting a "must use unstable" flag into
	 * pipe, and during bind you OR the two pipes' values
	def onlyPOS(posValue: String, prefix: Boolean = true) = new Transform[Token, Token]("pos="+posValue, (d: Document, t: Token) => {
		if(prefix && (t.getPosTag.toLowerCase.startsWith(posValue.toLowerCase))) Seq(t)
		else if(!prefix && (t.getPosTag equalsIgnoreCase posValue)) Seq(t)
		else Seq()
	})
	 */

	val onlyNEs = new Transform[Token, Token]("onlyNEs", (d: Document, t: Token) => {
		if("O" equalsIgnoreCase t.getNerTag) Seq()
		else Seq(t)
	})
	
	val treeToValue = new Transform[Tree, String]("tree2value", (d: Document, t: Tree) => { assert(t.value != null); t.value })
	
	val joinStr = new Transform[Seq[String], String]("joinStr", (d: Document, st: Seq[String]) => {
		//println("[joinStr] in=%s out=%s".format(st, st.mkString))
		st.mkString
	})
	
}


