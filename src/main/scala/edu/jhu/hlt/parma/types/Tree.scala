// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.types

import edu.jhu.hlt.parma.util.AnnotationAligner
import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

object TreeUtils {
	def sExpressionForParse(parse: Tree): String = {
		def helper(node: Tree, sb: StringBuilder) {
			if(node.isLeaf) sb.append(node.value)
			else {
				sb.append("(" + node.value)
				for(c <- node.children) {
					sb.append(" ")
					helper(c, sb)
				}
				sb.append(")")
			}
		}
		val sb = new StringBuilder
		helper(parse, sb)
		sb.toString
	}
}

object TreeBuilder {
	def from(tree: edu.jhu.hlt.concrete.Concrete.Parse.Constituent, sentence: Sentence): Tree =
		from(tree, sentence, RootTree, 0)
	private def from(tree: edu.jhu.hlt.concrete.Concrete.Parse.Constituent, sentence: Sentence, parent: Tree, depth: Int): ParmaTreeMutable = {
		assert(tree != null)
		assert(sentence != null)
		val hIdx = tree.getHeadChildIndex

		// 0 kids => leaf
		// only leaves should have no head
		val isLeaf = hIdx < 0
		assert((hIdx < 0) == (tree.getChildCount == 0))

		val t = new ParmaTreeMutable(sentence, tree.getTag, parent, depth)
		for((c, idx) <- tree.getChildList.zipWithIndex) {
			val kid = from(c, sentence, t, depth + 1)
			t.addChild(kid)
			val cTRS = c.getTokenSequence
			val cToks = cTRS.getTokenIndexList
			if(cToks.size == 1) {	// leaf
				kid.setHead(None)
				kid.setHeadToken(sentence(cToks(0)))
			}
			if(idx == hIdx) {
				assert(!isLeaf)
				t.setHead(Some(kid))
				t.setHeadToken(kid.headToken)
			}
		}

		// by the time we get here, everything below
		// will be completely build OR a leaf
		if(!isLeaf) t.setHeadToken(t.head.get.headToken)

		t
	}
}

class ParmaTree(
		override val sentence: Sentence,
		override val value: String,
		override val parent: Tree,
		override val depth: Int)
		extends Tree {
	override def children: Seq[Tree] = throw new RuntimeException("use ParmaTreeMutable")
	override def head: Option[Tree] = throw new RuntimeException("use ParmaTreeMutable")
	override def headToken: Token = throw new RuntimeException("use ParmaTreeMutable")
}

/**
 * use this when building ParmaTrees,
 * which have immutable fields and are difficult to build
 */
sealed class ParmaTreeMutable(
		override val sentence: Sentence,
		override val value: String,
		override val parent: Tree,
		override val depth: Int)
		extends ParmaTree(sentence, value, parent, depth) {

	private[this] var seen = false			// once seen, state cannot change
	private[this] val kids = new ArrayBuffer[Tree]
	private[this] var headTree: Option[Tree] = null
	private[this] var headTok: Token = null

	def addChild(t: Tree) {
		//assert(!seen)
		kids += t
	}
	def setHead(t: Option[Tree]) {
		//assert(!seen)
		headTree = t
	}
	def setHeadToken(tok: Token) {
		//assert(!seen)
		headTok = tok
	}
	override def children: Seq[Tree] = {
		seen = true
		kids.toSeq
	}
	override def head: Option[Tree] = {
		assert(headTree != null)
		seen = true
		headTree
	}
	override def headToken: Token = {
		assert(headTok != null)
		seen = true
		headTok
	}
}

object RootTree extends Tree {
	override def value: String = throw new RuntimeException("don't call me!")
	override def children: Seq[Tree] = throw new RuntimeException("don't call me!")
	override def parent: Tree = throw new RuntimeException("don't call me!")
	override def depth: Int = throw new RuntimeException("don't call me!")
	override def head: Option[Tree]  = throw new RuntimeException("don't call me!")
	override def headToken: Token  = throw new RuntimeException("don't call me!")
	override def sentence: Sentence  = throw new RuntimeException("don't call me!")
}

trait Tree {

	/**
	 * either a CFG rule name like "NP" for non-terminals
	 * or a word for terminals
	 */
	def value: String

	/**
	 * for leaves, return an empty Seq
	 */
	def children: Seq[Tree]

	/**
	 * topmost node is RootTree, and
	 * RootTree does not have a parent
	 * (or really any other functionality associated with it)
	 */
	def parent: Tree

	/**
	 * children of root have depth 0
	 */
	def depth: Int

	/**
	 * for leaves this returns None
	 * for all other nodes, returns the head child
	 */
	def head: Option[Tree]

	/**
	 * if this is a leaf node, then this should
	 * return the token generated by this node
	 */
	def headToken: Token

	/**
	 * the sentence that this parse tree is over
	 * the tokens in this sentence should match up
	 * with the leaves in this tree
	 */
	def sentence: Sentence

	def covers(m: Mention): Boolean = covers(m.getStartTokenIdx, m.getEndTokenIdx-1)
	def covers(left: Int, right: Int): Boolean = {	// left and right are both token indices, i.e. right is not exclusive
		leftmostChild.headToken.index <= left &&
			right <= rightmostChild.headToken.index
	}

	def liesIn(m: Mention): Boolean = liesIn(m.getStartTokenIdx, m.getEndTokenIdx)
	def liesIn(left: Int, right: Int): Boolean = {	// right is exclusive
		left <= leftmostChild.headToken.index &&
			rightmostChild.headToken.index < right
	}

	def dominates(tok: Token): Boolean = {
		val l = leftmostChild.headToken
		val r = rightmostChild.headToken
		l.index <= tok.index && tok.index <= r.index
	}

	def width: Int = {
		val l = leftmostChild.headToken.index
		val r = rightmostChild.headToken.index
		assert(l <= r)
		r - l + 1
	}

	def dominationPath(tok: Token): Seq[Tree] = {
		if(!dominates(tok))
			throw new RuntimeException("%s does not dominate %s".format(this, tok))
		if(isLeaf) Seq(this)
		else {
			for(t <- children)
				if(t.dominates(tok))
					return Seq(this) ++ t.dominationPath(tok)
			throw new RuntimeException("shouldnt have gotten here!")
		}
	}

	def isLeaf: Boolean = children.size == 0

	def leftmostChild: Tree = {
		if(isLeaf) this
		else children.head.leftmostChild
	}

	def rightmostChild: Tree = {
		if(isLeaf) this
		else children.last.rightmostChild
	}

	/**
	 * all leaves under this subtree
	 */
	def leaves: Seq[Token] = {
		val l = leftmostChild.headToken
		val r = rightmostChild.headToken
		sentence.tokens.slice(l.index, r.index+1)
	}

	def leafNodes: Seq[Tree] = {
		if(isLeaf) Seq(this)
		else children.flatMap(_.leafNodes)
	}

	/**
	 * returns a mention representing the tokens spanned
	 * by this tree
	 */
	def mentionSpan: Mention = {
		val l = leftmostChild.headToken
		val r = rightmostChild.headToken
		val h = headToken
		// Mentions end index is non-inclusive, hence
		// rightmostChild.index + 1
		assert(l.index <= h.index && h.index < r.index+1, "left=%d head=%d right=%d".format(l.index, h.index, r.index))
		MentionBuilder.from(sentence, l.index, r.index+1, h.index)
	}

	/**
	 * returns the text spanned by this tree
	 */
	def textSpan: String = leaves.map(_.getWord).mkString(" ")

	/**
	 * returns all nodes (internal and leaf) under this tree,
	 * including this tree
	 */
	def nodes: Seq[Tree] = {
		if(isLeaf) Seq(this)
		else this +: children.flatMap(_.nodes)
	}

	def topDownTraversal: Seq[Tree] = nodes.sortBy(_.depth)	// TODO more efficient with a queue...

}

