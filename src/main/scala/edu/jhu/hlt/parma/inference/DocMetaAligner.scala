// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.inference

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util.{ Misc, Profiler}
import scala.collection.JavaConversions._
import scala.collection.mutable.{Buffer, ArrayBuffer}
import edu.jhu.hlt.parma.feature_interfaces.AlignmentSimilarity
import cc.mallet.types.Alphabet

/**
 * this is a way to reuse some logic that is common to all
 * train/predict routines in an alignment model
 */
object DocMetaAligner {
	
	def allPossibleAlignments(c: Context): Buffer[Alignment] =
		allPossibleAlignments(c.report, c.passage)
	def allPossibleAlignments(report: Document, passage: Document): Buffer[Alignment] =
		allPossiblePredAlignments(report, passage) ++ allPossibleArgCorefAlignments(report, passage)
		
	def allPossiblePredAlignments(c: Context): Buffer[PredicateAlignment] =
		allPossiblePredAlignments(c.report, c.passage)
	def allPossiblePredAlignments(report: Document, passage: Document): Buffer[PredicateAlignment] = {
		Profiler.startTask("allPossiblePredAlignments")
		val alignments = new ArrayBuffer[PredicateAlignment]
		report.predicates.foreach(reportPred => {
			passage.predicates.foreach(passagePred => {
				alignments += PredicateAlignment(reportPred, passagePred)
			})
		})
		Profiler.endTask("allPossiblePredAlignments")
		alignments
	}

	def predAlignmentGrid(c: Context): IndexedSeq[IndexedSeq[PredicateAlignment]] =
		predAlignmentGrid(c.report, c.passage)
	def predAlignmentGrid(report: Document, passage: Document): IndexedSeq[IndexedSeq[PredicateAlignment]] = {
		val rp = report.predicates
		val pp = passage.predicates
		(0 until rp.size).map(i =>
			(0 until pp.size).map(j =>
				new PredicateAlignment(rp(i), pp(j)))
		).toIndexedSeq
	}

	def argAlignmentGrid(c: Context): IndexedSeq[IndexedSeq[ArgCorefAlignment]] =
		argAlignmentGrid(c.report, c.passage)
	def argAlignmentGrid(report: Document, passage: Document): IndexedSeq[IndexedSeq[ArgCorefAlignment]] = {
		val ra = report.corefs
		val pa = passage.corefs
		(0 until ra.size).map(i =>
			(0 until pa.size).map(j =>
				new ArgCorefAlignment(ra(i), pa(j)))
		).toIndexedSeq
	}
	
	def allPossibleArgCorefAlignments(c: Context): Buffer[ArgCorefAlignment] =
		allPossibleArgCorefAlignments(c.report, c.passage)
	def allPossibleArgCorefAlignments(report: Document, passage: Document): Buffer[ArgCorefAlignment] = {
		Profiler.startTask("allPossibleArgCorefAlignments")
		val alignments = new ArrayBuffer[ArgCorefAlignment]
		report.corefs.foreach(ra => {
			passage.corefs.foreach(pa => {
				alignments += ArgCorefAlignment(ra, pa)
			})
		})
		Profiler.endTask("allPossibleArgCorefAlignments")
		alignments
	}

	/**
	 * efficiently computes how many alignments are possible
	 */
	def numPossibleAlignments(da: DocAlignment): Int =
		numPossibleAlignments(da.report, da.passage)
	def numPossibleAlignments(c: Context): Int =
		numPossibleAlignments(c.report, c.passage)
	def numPossibleAlignments(report: Document, passage: Document): Int = {
		val rp = report.predicates.size
		val pp = passage.predicates.size
		val ra = report.corefs.size
		val pa = passage.corefs.size
		(rp * pp) + (ra * pa)
	}
}

