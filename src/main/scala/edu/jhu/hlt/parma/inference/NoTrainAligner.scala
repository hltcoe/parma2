// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.inference

import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.evaluation._
import edu.jhu.hlt.parma.features.FeatureLoader
import edu.jhu.hlt.parma.feature_interfaces.BinaryAlignmentSimilarity
import java.io.File

class NTFR(
	val features: Map[Alignment, Boolean],
	val report: Document,
	val passage: Document,
	val domain: Option[String],
	val controller: NoTrainAligner)
	extends FeatureRepresentation {
	override def inspectFeatures: Option[Map[Alignment, SVec]] =
		Some(features.mapValues(b => SVec(0, if(b) 1d else 0d)))
	override def inspectScores: Option[Map[Alignment, Double]] =
		Some(features.mapValues(b => if(b) 1d else 0d))
}

class NoTrainAligner(val feature: BinaryAlignmentSimilarity) extends InferenceEngine[NTFR] {

	// right now this class is set up to look at
	// one binary feature
	def this() = this({
		val name = ParmaConfig.getString("inference.notrain.feature")
		FeatureLoader.loadFeatureFunction(name) match {
			case bf: BinaryAlignmentSimilarity => bf
			case _ => throw new RuntimeException("you must provide a binary feature")
		}
	})

	override def featureName(index: Int): String = {
		assert(index == 0)
		feature.name
	}
	override val parameters = new DVec(1d)

	override def preTrainCalibrate(examples: Seq[DocAlignment]) {}
	override def train(examples: Seq[DocAlignmentWithFeatures[NTFR]]) {}
	override def postTrainCalibrate(examples: Seq[DocAlignmentWithFeatures[NTFR]], loss: EvaluationLoss) {}
	
	override def computeFeatures(report: Document, passage: Document, domain: Option[String] = None): NTFR = {
		val map = DocMetaAligner.allPossibleAlignments(report, passage)
			.map(a => (a, feature.fires(a, report, passage)))
			.toMap
		new NTFR(map, report, passage, domain, this)
	}
	
	override def align(frs: Seq[NTFR]): Seq[DocAlignment] = {
		frs.map(fr => {
			val id = "NT_%s_%s_%s".format(feature.name, fr.report.id, fr.passage.id)
			val alignments = fr.features.filter(_._2).keySet
			new DocAlignment(id, fr.domain, fr.report, fr.passage, alignments, Set())
		})
	}

	override def writeoutParameters(f: File) {
		val bw = FileUtils.getWriter(f)
		bw.write("feature = " + feature.name + "\n")
		bw.close
	}

	override def readParameters(f: File) {
		throw new RuntimeException("implement me")
	}
	
}

object NoTrainAligner {
	import edu.jhu.hlt.parma.features.LemmaMatch
	val lemmaMatch = new NoTrainAligner(new LemmaMatch)
}

