// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.evaluation

import edu.jhu.hlt.parma.util.Describe
import edu.jhu.hlt.parma.inference.DocMetaAligner
import edu.jhu.hlt.parma.types._
import java.util.logging._

/**
 * NOTE: that generous versions of precision, recall, and F1
 * reduce to their regular counterparts if there are only sure alignments
 *
 * NOTE: you cannot give write down TP, FP, FN, and TN counts in terms
 * that will give you generous precision and recalls, so use the generous calls instead
 * The TP, FP, FN, and TN counts will be computed as if we are dealing with the
 * possible versions on both the gold and hyp side.
 */
object SetBasedEvaluator {

	val log = Logger.getLogger(this.getClass.getName)

	private def checkInstance(inst: Instance[DocAlignment]) {
		assert(inst.hyp.exactlyPossibleAlignments.size == 0)
		assert(inst.hyp.possibleAlignments.size == inst.hyp.sureAlignments.size)
		assert(inst.gold.possibleAlignments.size >= inst.gold.sureAlignments.size)
	}


	sealed abstract class AlignmentsToTake
	case object All extends AlignmentsToTake
	case object Preds extends AlignmentsToTake
	case object Args extends AlignmentsToTake


	def truePos(inst: Instance[DocAlignment], take: AlignmentsToTake = All): Int = take match {
		case All =>
			(inst.gold.possibleAlignments & inst.hyp.possibleAlignments).size
		case Preds =>
			(inst.gold.possiblePredicateAlignments.toSet & inst.hyp.possiblePredicateAlignments.toSet).size
		case Args =>
			(inst.gold.possibleArgCorefAlignments.toSet & inst.hyp.possibleArgCorefAlignments.toSet).size
	}
	
	def trueNeg(inst: Instance[DocAlignment], take: AlignmentsToTake = All): Int = {
		// this is mega-inefficient, i think i really want that
		// "ExplicitDocAlignment" now. or possibly just push a "def notAligned: Seq[Alignment]"
		// method into DocAlignment, which naivey will be implemented as DocMetaAligner.allPossible - aligned
		val r = inst.gold.report
		val p = inst.gold.passage
		val all: Set[Alignment] = take match {
			case All =>
				DocMetaAligner.allPossibleAlignments(r, p).toSet
			case Preds =>
				DocMetaAligner.allPossiblePredAlignments(r, p).toSet
			case Args =>
				DocMetaAligner.allPossibleArgCorefAlignments(r, p).toSet
		}
		(all -- (inst.gold.possibleAlignments | inst.hyp.possibleAlignments)).size
	}

	def falsePos(inst: Instance[DocAlignment], take: AlignmentsToTake = All): Int = {
		val hyp: Set[Alignment] = take match {
			case All =>
				inst.hyp.possibleAlignments
			case Preds =>
				inst.hyp.possiblePredicateAlignments.toSet
			case Args =>
				inst.hyp.possibleArgCorefAlignments.toSet
		}
		(hyp -- inst.gold.possibleAlignments).size
	}

	// swap gold and hyp, then its the same as false pos
	def falseNeg(inst: Instance[DocAlignment], take: AlignmentsToTake = All): Int =
		falsePos(Instance(inst.gold, inst.hyp), take)


	def hamming(inst: Instance[DocAlignment],
			falsePosPenalty: Double = 1d, falseNegPenalty: Double = 1d,
			normalize: Boolean = true, take: AlignmentsToTake = All): Double = {

		checkInstance(inst)

		val fp = falsePos(inst, take)
		val fn = falseNeg(inst, take)
		val unNormalized = fp * falsePosPenalty + fn * falseNegPenalty
		
		if(normalize) {
			val rp = inst.gold.report.predicates.size
			val pp = inst.gold.passage.predicates.size
			val ra = inst.gold.report.corefs.size
			val pa = inst.gold.passage.corefs.size
			val maxFalsePos = take match {
				case All =>
					val numPossibleAlignments = (rp * pp) + (ra * pa)
					numPossibleAlignments - inst.gold.possibleAlignments.size
				case Preds =>
					val numPossibleAlignments = rp * pp
					numPossibleAlignments - inst.gold.possiblePredicateAlignments.size
				case Args =>
					val numPossibleAlignments = ra * pa
					numPossibleAlignments - inst.gold.possibleArgCorefAlignments.size
			}
			val maxFalseNeg = take match {
				case All =>
					inst.gold.possibleAlignments.size
				case Preds =>
					inst.gold.possiblePredicateAlignments.size
				case Args =>
					inst.gold.possibleArgCorefAlignments.size
			}
			val normalizer = maxFalsePos * falsePosPenalty + maxFalseNeg * falseNegPenalty
			unNormalized / normalizer
		}
		else unNormalized
	}

	// page 602 on http://aclweb.org/anthology-new/J/J08/J08-4005.pdf
	def generousPrecision(inst: Instance[DocAlignment], take: AlignmentsToTake = All): Double = generousPrecisionWithWeight(inst, take)._1
	def generousPrecisionWithWeight(inst: Instance[DocAlignment], take: AlignmentsToTake = All): (Double, Int) = {
		checkInstance(inst)
		val (h, g): (Set[Alignment], Set[Alignment]) = take match {
			case All =>
				(inst.hyp.sureAlignments, inst.gold.possibleAlignments)
			case Preds =>
				(inst.hyp.surePredicateAlignments.toSet, inst.gold.possiblePredicateAlignments.toSet)
			case Args =>
				(inst.hyp.sureArgCorefAlignments.toSet, inst.gold.possibleArgCorefAlignments.toSet)
		}
		if(h.size == 0) (1d, 0)
		else ((h & g).size.toDouble / h.size, h.size)
	}

	// page 602 on http://aclweb.org/anthology-new/J/J08/J08-4005.pdf
	def generousRecall(inst: Instance[DocAlignment], take: AlignmentsToTake = All): Double = generousRecallWithWeight(inst, take)._1
	def generousRecallWithWeight(inst: Instance[DocAlignment], take: AlignmentsToTake = All): (Double, Int) = {
		checkInstance(inst)
		val (h, g): (Set[Alignment], Set[Alignment]) = take match {
			case All =>
				(inst.hyp.possibleAlignments, inst.gold.sureAlignments)
			case Preds =>
				(inst.hyp.possiblePredicateAlignments.toSet, inst.gold.surePredicateAlignments.toSet)
			case Args =>
				(inst.hyp.possibleArgCorefAlignments.toSet, inst.gold.sureArgCorefAlignments.toSet)
		}
		if(g.size == 0) (1d, 0)
		else ((h & g).size.toDouble / g.size, g.size)
	}
	
	def generousF1(instance: Instance[DocAlignment], take: AlignmentsToTake = All): Double = {
		val p = generousPrecision(instance, take)
		val r = generousRecall(instance, take)
		if(p + r == 0d) 0d
		else 2d*p*r / (p+r)
	}
	
	def microAvg[T](instances: Seq[Instance[T]], perf: Instance[T] => Double, weight: T => Double): Double = {
		assert(instances.size > 0)
		var num = 0d
		var denom = 0d
		instances.foreach(inst => {
			val w = weight(inst.gold)
			val p = perf(inst)
			num += w*p
			denom += w
		})
		num / denom
	}

	def macroAvg[T](instances: Seq[Instance[T]], perf: Instance[T] => Double): Double = {
		microAvg(instances, perf, (gold: T) => 1d)
	}
}

