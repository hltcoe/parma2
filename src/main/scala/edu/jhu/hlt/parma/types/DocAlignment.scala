// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.types

import edu.jhu.hlt.parma.util.Describe
import edu.jhu.hlt.concrete.Concrete
import scala.collection.JavaConversions._

/**
 * just a dumb set of alignments!
 * please do not put logic in here
 */
class DocAlignment(
		val id: String,
		val domain: Option[String],
		val report: Document,
		val passage: Document,
		val sureAlignments: Set[Alignment],
		val exactlyPossibleAlignments: Set[Alignment]) {
	
	val possibleAlignments = sureAlignments | exactlyPossibleAlignments
	val context = new Context(report, passage)

	def predicateAlignments = surePredicateAlignments ++ possiblePredicateAlignments

	def argCorefAlignments = sureArgCorefAlignments ++ possibleArgCorefAlignments

	def surePredicateAlignments = sureAlignments.iterator.filter(_ match {
		case x: PredicateAlignment => true
		case _ => false
	}).asInstanceOf[Iterator[PredicateAlignment]]
	
	def possiblePredicateAlignments = possibleAlignments.iterator.filter(_ match {
		case x: PredicateAlignment => true
		case _ => false
	}).asInstanceOf[Iterator[PredicateAlignment]]
	
	def sureArgCorefAlignments = sureAlignments.iterator.filter(_ match {
		case x: ArgCorefAlignment => true
		case _ => false
	}).asInstanceOf[Iterator[ArgCorefAlignment]]
	
	def possibleArgCorefAlignments = possibleAlignments.iterator.filter(_ match {
		case x: ArgCorefAlignment => true
		case _ => false
	}).asInstanceOf[Iterator[ArgCorefAlignment]]
	
	override def toString: String = Describe.docAlignment(this)

	override def hashCode: Int = id.hashCode
	override def equals(other: Any): Boolean = {
		println("you probably don't want to be calling equals on DocAlignment!")
		if(other.isInstanceOf[DocAlignment]) {
			val o = other.asInstanceOf[DocAlignment]
			id == o.id &&
				report == o.report &&
				passage == o.passage &&
				sureAlignments == o.sureAlignments &&
				possibleAlignments == o.possibleAlignments
		}
		else false
	}
}

class ParametricDocAlignment[+D <: Document](
		override val id: String,
		override val domain: Option[String],
		override val report: D,
		override val passage: D,
		override val sureAlignments: Set[Alignment],
		override val exactlyPossibleAlignments: Set[Alignment])
		extends DocAlignment(id, domain, report, passage, sureAlignments, exactlyPossibleAlignments) {
}

/**
 * TODO
 * keeps track of alignments that are not positive
 * TODO make parametric?
 * TODO add scores?
class ExplicitDocAlignment(
		override val id: String,
		override val domain: Option[String],
		override val report: Document,
		override val passage: Document,
		override val sureAlignments: Set[Alignment],
		override val exactlyPossibleAlignments: Set[Alignment],
		val negativeAlignments: Set[Alignment])
		extends DocAlignment {
}
 */

