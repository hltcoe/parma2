package edu.jhu.hlt.parma.experiments.acl2014

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.evaluation._
import edu.jhu.hlt.parma.inference.{ DocMetaAligner, NoTrainAligner }
import collection.mutable.ArrayBuffer

/**
 * used to compare similarity of training instances for domain adaptation
 */
class DAStats(val das: Seq[DocAlignment]) {

	// we will define similarity over a collection of DAs by
	// their distribution over these variables
	val statsWeCareAbout = {
		val f = new ArrayBuffer[DocAlignment => Double]

		// size of the alignments
		f += ((da: DocAlignment) => DocMetaAligner.allPossiblePredAlignments(da.context).size.toDouble)
		f += ((da: DocAlignment) => DocMetaAligner.allPossibleArgCorefAlignments(da.context).size.toDouble)

		// proportion of positive instances
		f += ((da: DocAlignment) => {
			val denom = DocMetaAligner.allPossiblePredAlignments(da.context).size.toDouble + 1d
			val num = da.possiblePredicateAlignments.size.toDouble + 1d
			num / denom
		})
		f += ((da: DocAlignment) => {
			val denom = DocMetaAligner.allPossibleArgCorefAlignments(da.context).size.toDouble + 1d
			val num = da.possibleArgCorefAlignments.size.toDouble + 1d
			num / denom
		})

		// lemma match score
		f += ((da: DocAlignment) => {
			val daLemmaMatch: DocAlignment = NoTrainAligner.lemmaMatch.align(da.report, da.passage, da.domain)
			MicroPrecision(Seq(Instance(daLemmaMatch, da)))
		})
		f += ((da: DocAlignment) => {
			val daLemmaMatch: DocAlignment = NoTrainAligner.lemmaMatch.align(da.report, da.passage, da.domain)
			MicroRecall(Seq(Instance(daLemmaMatch, da)))
		})

		f.toIndexedSeq
	}

	def mean(vals: Seq[Double]): Double = vals.sum / vals.size
	def stdDev(vals: Seq[Double]): Double = {
		require(vals.size > 1)
		val mu = mean(vals)
		val devs = vals.map(_ - mu).map(x => x*x)
		math.sqrt(devs.sum / devs.size)
	}
	val means: IndexedSeq[Double] = statsWeCareAbout.map(f => mean(das.map(f)))
	val stdDevs: IndexedSeq[Double] = statsWeCareAbout.map(f => stdDev(das.map(f)))

	/**
	 * compute a uniformly weighted z-score across all functions we care about
	 */
	def divergence(da: DocAlignment): Double = {
		var s = 0d
		for((f, i) <- statsWeCareAbout.zipWithIndex) {
			val x = f(da)
			val z = math.abs((x - means(i)) / stdDevs(i))
			s += z
		}
		s / statsWeCareAbout.size
	}
}

