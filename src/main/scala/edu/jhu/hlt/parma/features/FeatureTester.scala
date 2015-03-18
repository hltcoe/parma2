package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.input._
import edu.jhu.hlt.parma.inference._

object FeatureTester extends Logging2 {

	val showPreds = true
	val showArgs = true
	val showPos = true
	val showNeg = false
	val numDAs = 200
	val maxPA = 50
	val maxAA = 50

	def show(a: Alignment, idx: Int, ham: HierarchicalAlignmentModule, da: DocAlignment, label: String) {
		val pos = da.possibleAlignments.contains(a)
		log("%s-alignment-%d pos=%s".format(label, idx+1, pos))
		val features: SVec = ham.computeFeatures(a, da.report, da.passage, da.domain)
		ham.registerFeatures(Seq(features))	// TODO fix this in HAM/FeatureSet
		val maxIdx: Int = features.rawIndices.max
		log(Describe.alignment(a, da.report, da.passage, contextWordsEachSide=5))
		log(Describe.features(features, ham, compact=true))
		require(maxIdx == features.rawIndices.max)
	}

	def keep(a: Alignment, da: DocAlignment): Boolean = {
		val pos = da.possibleAlignments.contains(a)
		(pos && showPos) || (!pos && showNeg)
	}

	/**
	 * pass in feature names that you want to see
	 */
	def main(args: Array[String]) {
		
		var cfgFile = "parma.config"
		ParmaConfig.load(cfgFile)

		// pass in feature names
		if(args.length > 0)
			System.setProperty("features", args.mkString(" "))

		teeLogTo(StdOutLogger, new FileLogger("diagnostics/feature-tester.log"))

		val ham = new HierarchicalAlignmentModule
		val das: Seq[DocAlignment] = ConcreteDocAlignmentReader.EECB.daIter.take(numDAs).toSeq
		ham.preTrainCalibrate(das)

		for(da <- das) {

			log(da.id)

			if(showPreds)
				for((a, idx) <- DocMetaAligner.allPossiblePredAlignments(da.context).filter(keep(_, da)).zipWithIndex.take(maxPA))
					show(a, idx, ham, da, "pred")

			if(showArgs)
				for((a, idx) <- DocMetaAligner.allPossibleArgCorefAlignments(da.context).filter(keep(_, da)).zipWithIndex.take(maxAA))
					show(a, idx, ham, da, "arg")

			log("\n\n")
		}
	}

}


