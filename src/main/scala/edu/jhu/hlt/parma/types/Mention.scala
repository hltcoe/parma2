// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.types

import edu.jhu.hlt.parma.util._
import collection.JavaConversions._

class ParmaMention(
		override val getSentenceIdx: Int,
		override val getStartTokenIdx: Int,
		override val getEndTokenIdx: Int,
		override val getHeadTokenIdx: Int)
		extends Mention with Serializable

object MentionBuilder extends Logging2 {

	// assumes that this token ref sequence is indexed from 0 and a contiguous span
	def from(sentence: Sentence, trs: edu.jhu.hlt.concrete.Concrete.TokenRefSequence): Mention = {

		// scala does weird things about the return type of sliding depending on what you call it on
		//assert(trs.getTokenIndexList.sliding(2).forall(ij => ij._1 + 1 == ij._2))
		val l = trs.getTokenIndexList
		for(i <- 0 until l.size-1) {
			assert(l(i)+1 == l(i+1))
			assert(l(i) >= 0)
		}

		var h = trs.getAnchorTokenIndex
		val left = trs.getTokenIndexList.min
		val right = trs.getTokenIndexList.max + 1
		assert(left < right)
		if(h >= sentence.size)
			throw new RuntimeException("h = " + h)
		if(h < 0) {
			if(right > left+1)
				warn("no anchor token available, setting to last token!")
			h = right-1
		}
		if(left < 0 || left >= sentence.size || right < 0 || right > sentence.size || left >= right)
			throw new RuntimeException("left = " + left + ", right = " + right)
		new ParmaMention(sentence.index, left, right, h)
	}

	def from(sentence: Sentence, startTokenIdx: Int, endTokIdx: Int, headTokIdx: Int): Mention =
		from(sentence.index, startTokenIdx, endTokIdx, headTokIdx)
	def from(sentenceIdx: Int, startTokIdx: Int, endTokIdx: Int, headTokIdx: Int): Mention = {
		val isRepresentative = false
		assert(sentenceIdx >= 0)
		assert(startTokIdx >= 0)
		assert(startTokIdx < endTokIdx)
		assert(startTokIdx <= headTokIdx && headTokIdx < endTokIdx, "start=%d head=%d end=%d".format(startTokIdx, headTokIdx, endTokIdx))
		new ParmaMention(sentenceIdx, startTokIdx, endTokIdx, headTokIdx)
	}

	/** makes a width-1 mention */
	def from(sent: Sentence, token: Token): Mention =
		from(sent.index, token.index)

	/** makes a width-1 mention */
	def from(sentenceIdx: Int, token: Token): Mention =
		from(sentenceIdx, token.index, token.index+1, token.index)
	
	/** makes a width-1 mention */
	def from(sentenceIdx: Int, tokenIdx: Int): Mention =
		from(sentenceIdx, tokenIdx, tokenIdx+1, tokenIdx)
}

trait Mention {
	def getHeadTokenIdx: Int
	def getStartTokenIdx: Int
	def getEndTokenIdx: Int
	def getSentenceIdx: Int

	override def toString: String = "(Mention sent=%d %d-%d h=%d)"
		.format(getSentenceIdx, getStartTokenIdx, getEndTokenIdx, getHeadTokenIdx)
	
	override def hashCode: Int = {
		var hc = 0
		hc = (hc << 8) | getStartTokenIdx
		hc = (hc << 8) | getEndTokenIdx
		hc = (hc << 8) | getHeadTokenIdx
		hc = (hc << 8) | getSentenceIdx
		hc
	}
	override def equals(other: Any): Boolean = {
		if(other.isInstanceOf[Mention]) {
			val o = other.asInstanceOf[Mention]
			getStartTokenIdx == o.getStartTokenIdx &&
				getEndTokenIdx == o.getEndTokenIdx &&
				getHeadTokenIdx == o.getHeadTokenIdx &&
				getSentenceIdx == o.getSentenceIdx
		}
		else false
	}

	def width: Int = {
		val w = getEndTokenIdx - getStartTokenIdx
		assert(w > 0)
		w
	}

	def contains(t: Token): Boolean = getStartTokenIdx <= t.index && t.index < getEndTokenIdx

	def tokenRange: (Int, Int) = (getStartTokenIdx, getEndTokenIdx)
}

object Mention {
	import edu.jhu.hlt.concrete.Concrete.TokenRefSequence
	def toTokenRefSeq(mention: Mention, sent: RichConcreteSent): TokenRefSequence = {
		assert(sent.concreteSent.getTokenizationList.size == 1)
		val tid = sent.concreteSent.getTokenizationList.get(0).getUuid
		val b = TokenRefSequence.newBuilder
			.setTokenizationId(tid)
			.setAnchorTokenIndex(mention.getHeadTokenIdx)
		for(i <- mention.getStartTokenIdx until mention.getEndTokenIdx)
			b.addTokenIndex(i)
		b.build
	}
}


