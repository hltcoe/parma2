// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.experiments

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.input._
import edu.jhu.hlt.parma.evaluation._
import edu.jhu.hlt.parma.inference._
import edu.jhu.hlt.parma.features._
import edu.jhu.hlt.parma.diagnostics.BadAlignmentDiagnostics
import util.Random
import collection.mutable.ArrayBuffer

class RFLemmaDevSet extends Experiment[NoTrainAligner] {
	lazy val devAlignments = new RothFrankDocAlignmentReader(true, false) getDocAlignments
	override def rawData = new DocAlignmentCorpus("RF_dev", Seq(), Seq(), devAlignments.toSeq)
	override def inferenceEngine = new NoTrainAligner(new LemmaMatch)
}

class RFLemmaTestSet extends Experiment[NoTrainAligner] {
	lazy val testAlignments = new RothFrankDocAlignmentReader(false, true) getDocAlignments
	override def rawData = new DocAlignmentCorpus("RF_test", Seq(), Seq(), testAlignments.toSeq)
	override def inferenceEngine = new NoTrainAligner(new LemmaMatch)
}

class RFOnlyExperiment extends Experiment[HierarchicalAlignmentModule] {
	lazy val trainAlignments = new RothFrankDocAlignmentReader(true, false) getDocAlignments
	lazy val testAlignments = new RothFrankDocAlignmentReader(false, true) getDocAlignments
	override val rawData = new DocAlignmentCorpus("trainRF_testRF", trainAlignments.toSeq, Seq(), testAlignments.toSeq)
	override def inferenceEngine = new HierarchicalAlignmentModule
}


class EECBCVExperiment extends Experiment[HierarchicalAlignmentModule] {
	val reader = ConcreteDocAlignmentReader.EECB
	val (dev, test) = DocAlignmentCorpus.randomSplit(reader.getDocAlignments.toSeq, ParmaConfig.getDouble("experiments.propDev"))
	override def rawData = new DocAlignmentCorpus("eecb", Seq(), dev, test)
  	override def inferenceEngine = new HierarchicalAlignmentModule
	override def evaluationSplits[T](c: Corpus[T])(implicit asDA: T => DocAlignment) =
		EvaluationSplits.crossValidation(ParmaConfig.getInt("experiments.cvFolds"))(c)
}

class EECBLemmaExperiment extends Experiment[NoTrainAligner] {
	override def rawData = new DocAlignmentCorpus("eecb", Seq(), Seq(), EECBDocAlignmentReader.getDocAlignments.toSeq)
  	override def inferenceEngine = new NoTrainAligner(new LemmaMatch)
}

class EECBTrainFrankTestExperiment extends Experiment[HierarchicalAlignmentModule] {
	lazy val trainAlignments = EECBDocAlignmentReader.getDocAlignments
	lazy val testAlignments = new RothFrankDocAlignmentReader(false, true) getDocAlignments
	override val rawData = new DocAlignmentCorpus("trainEECB_testRFdev", trainAlignments.toSeq, Seq(), testAlignments.toSeq)
	override def inferenceEngine = new HierarchicalAlignmentModule
}

class MixtureTrainRFTestExperimentWithLDC extends MixtureTrainRFTestExperiment {
	override def train: Seq[DocAlignment] =
		super.train ++ ConcreteDocAlignmentReader.MTC.getDocAlignments.take(300)
}


/**
 * use this experiment just to train a model, not evalutate
 */
class TurkerEvalTrain extends Experiment[HierarchicalAlignmentModule] {
	override def inferenceEngine = new HierarchicalAlignmentModule
	//private val dev = Seq[DocAlignment]()	//new RothFrankDocAlignmentReader(true, false).getDocAlignments.toSeq
	//private val train = EECBDocAlignmentReader.getDocAlignments.take(3)
	private val dev = new RothFrankDocAlignmentReader(true, false).getDocAlignments.toSeq
	private val train = EECBDocAlignmentReader.getDocAlignments ++ (new RothFrankDocAlignmentReader(false, true).getDocAlignments)
	override def rawData = new DocAlignmentCorpus("turker-eval-train", train, dev, Seq())
}


class MixtureTrainRFTestExperiment extends Experiment[HierarchicalAlignmentModule] {

	override def inferenceEngine = new HierarchicalAlignmentModule

	val test = new RothFrankDocAlignmentReader(false, true).getDocAlignments.toSeq
	val rfDev = new RothFrankDocAlignmentReader(true, false).getDocAlignments.toSeq
	val dev = rfDev ++
			Random.shuffle(EECBDocAlignmentReader.getDocAlignments.toSeq).take(10) ++
			Random.shuffle(ConcreteDocAlignmentReader.MTC.getDocAlignments).take(10)
	def train: Seq[DocAlignment] = Random.shuffle(EECBDocAlignmentReader.getDocAlignments.toSeq).take(300)
	override def rawData = new DocAlignmentCorpus("trainMix_testRF", train, dev, test)

	override def evaluationSplits[T](c: Corpus[T])(implicit asDocAlignment: T => DocAlignment): Seq[Corpus[T]] = {
		val rfDomain = new RothFrankDocAlignmentReader(true, false).domain
		val (devOOD, devID) = c.dev.partition(_.domain != Some(rfDomain))
		assert(devOOD.size > 0)	// "out of domain"
		assert(devID.size > 0)	// "in domain"
		Seq(1, 5, 15).flatMap(devReps => {
			val dv = devOOD ++ (1 to devReps).flatMap(i => devID)
			Seq(5, 15, 50).map(trainReps => {
				val tr = c.train ++ (1 to trainReps).flatMap(i => devID)
				val id = "%s-dev%d-train%d".format(c.id, devReps, trainReps)
				new Corpus(id, tr, dv, c.test)
			})
		})
	}
}

class MTCLemma extends Experiment[NoTrainAligner] {
	override def rawData = new DocAlignmentCorpus("MTCLemma", Seq(), Seq(), ConcreteDocAlignmentReader.MTC.getDocAlignments)
  	override def inferenceEngine = new NoTrainAligner(new LemmaMatch)
}

class MTCCV extends Experiment[HierarchicalAlignmentModule] {
	override def rawData = {
		val (dev, test) = DocAlignmentCorpus.randomSplit(ConcreteDocAlignmentReader.MTC.getDocAlignments, ParmaConfig.getDouble("experiments.propDev"))
		new DocAlignmentCorpus("MTCCV", Seq(), dev, test)
	}
	override def inferenceEngine = new HierarchicalAlignmentModule
	override def evaluationSplits[T](c: Corpus[T])(implicit asDA: T => DocAlignment) =
		EvaluationSplits.crossValidation(ParmaConfig.getInt("experiments.cvFolds"))(c)
}


class PolyTrain extends Experiment[HierarchicalAlignmentModule] {
	override def rawData = {
		val buf = new ArrayBuffer[DocAlignment]
		buf ++= ConcreteDocAlignmentReader.EECB.getDocAlignments
		buf ++= ConcreteDocAlignmentReader.RF.getDocAlignments
		buf ++= ConcreteDocAlignmentReader.GV.getDocAlignments
		buf ++= ConcreteDocAlignmentReader.MTC.getDocAlignments
		val (test, trainDev) = DocAlignmentCorpus.randomSplit(buf.toSeq, ParmaConfig.getDouble("experiments.propTest"))
		val (dev, train) = DocAlignmentCorpus.randomSplit(trainDev, ParmaConfig.getDouble("experiments.propDev"))
		new DocAlignmentCorpus("PolyTrain", train, dev, test)
	}
	override def inferenceEngine = new HierarchicalAlignmentModule
}

class GVCV extends Experiment[HierarchicalAlignmentModule] {
	override def rawData = {
		val all = ConcreteDocAlignmentReader.GV.getDocAlignments
		val (train, test) = DocAlignmentCorpus.randomSplit(all, 0.6d)
		new DocAlignmentCorpus("GVCV", train, Seq(), test)
	}
	override def inferenceEngine = new HierarchicalAlignmentModule
	//override def evaluationSplits[T](c: Corpus[T])(implicit asDA: T => DocAlignment) =
	//	EvaluationSplits.crossValidation(ParmaConfig.getInt("experiments.cvFolds"))(c)
}


