package edu.jhu.hlt.parma.experiments.acl2014

import edu.jhu.hlt.parma.evaluation.PairedBootstrap.MutablePairedBootstrap
import edu.jhu.hlt.parma.inference.DocMetaAligner
import edu.jhu.hlt.parma.input._
import edu.jhu.hlt.parma.util._
import java.io.File
import io.Source
import util.Random
import collection.mutable.ArrayBuffer

object AblationResults {

	val oldWay = true	// if false, do virtual dev evaluation, otherwise look for dev/test results separately

	def main(args: Array[String]) {
		if(args.length < 2) {
			throw new IllegalArgumentException("please provide:\n" +
				"1) a results directory from the Ablation experiment\n" +
				"2) at least one dataset name to run (e.g. \"EECB\" or \"RF\")\n")
		}
		val rootDir = new File(args(0))
		for(i <- 1 until args.length) {
			new AblationResults(rootDir, args(i)).compareAgainstBaseline(oldWay)
			println()
		}
		/* i'm commandeering this main method
		val eecb = new AblationResults(rootDir, "EECB")
		args.drop(1).foreach(modelName =>
			eecb.learningCurves(modelName))
		*/
	}
}

/**
 * 1) scan over resulting log files and choose the best configuration for each model type (according to dev data)
 * 2) given one config's working dir per model,
 *    A) print the test performance
 *    B) pair up the baseline and other instance scores, perform bootstrap CI
 *
 * new gameplan:
 * in addition to doing the bootstrap test around the mean,
 * also show the instances scores in box-plots
 */
class AblationResults(val rootDir: File, val datasetName: String) extends Logging2 {

	require(rootDir.isDirectory, rootDir.getPath + " is not a directory!")
	warnIf(!Set("EECB", "RF").contains(datasetName), "dataset=" + datasetName + ", you probably want either EECB or RF")

	// TODO write out a latex table, or maybe a data digest for plotting in R?



	/*
oh man am i an idiot
i forgot to add to the dev set!
now i can't find the best performing system on dev data!

though, there is a way i can fix this
since i have all of the instance scores for the test data,
i can just partition them into dev and test
do the max over dev and the avg over test
the problem with this is that we've added variance to the estimate (test set is smaller than it would have been otherwise
we can fix this problem by averaging over many bootstrap samples of the partition (is this called jackknife?)

is there any reason not to always do this?
whether or not you want to do this depends on the relative strengths of two factors:
1) how much your model's performance increases with more training data
2) how much power you lose by having a smaller test set
	*/

	/**
	 * the idea here is if you accidentally forgot to include dev data,
	 * but you have instance-level scores for test data,
	 * you can't cheat and take the max_{hyperParams} avg score on test,
	 * so we use a bootstrap proceedure to estimate the score for the entire proceedure.
	 * this involves halucinating dev sets (drawn from test), and averaging over these halucinations.
	 * in the end, we do not have the performance of a given hyper param setting,
	 * which would require us to commit to one and never use any of the data used to make that decision for evaluation
	 * (which would lead to an incredibly small test set size).
	 * what we do have is a bootstrap estimate of the performance of this model under the finishing proceedure
	 * of maxing hyperparams on a dev set.
	 */
	def virtualDevDataEvaluation(modelName: String, hyperParamSweep: Seq[ModelScoreDir], baseline: ModelScoreDir, rand: Random) {
		val allDAids: Seq[String] = hyperParamSweep.head.testInstanceScores.map(_.daId)
		val bootstrapScores = new ArrayBuffer[Double]
		val baselineScores = new ArrayBuffer[Double]
		var nBeatBaseline = 0
		val nBootstraps = 1000
		for(i <- 1 to nBootstraps) {

			// partition all instances into dev and test
			val shufDAids = rand.shuffle(allDAids)
			val nDev = shufDAids.size / 2
			val (dev, test) = shufDAids.splitAt(nDev)
			val devSet = dev.toSet
			val testSet = test.toSet

			// choose a best hyperparam setting on dev data
			val bestModel = hyperParamSweep.maxBy(m => {
				val devScores = m.testInstanceScores.filter(inst => devSet.contains(inst.daId))
				devScores.map(_.perf("full-F1")).sum / devScores.size
			})

			// yield the peroformance of this model on test data
			val testScores = bestModel.testInstanceScores.filter(inst => testSet.contains(inst.daId))
			val avgTestScore = testScores.map(_.perf("full-F1")).sum / testScores.size
			bootstrapScores += avgTestScore

			val testScoresBaseline = baseline.testInstanceScores.filter(inst => testSet.contains(inst.daId))
			val avgTestScoreBaseline = testScoresBaseline.map(_.perf("full-F1")).sum / testScoresBaseline.size
			baselineScores += avgTestScoreBaseline
			if(avgTestScore > avgTestScoreBaseline)
				nBeatBaseline += 1
		}
		val avgScore = 100d * (bootstrapScores.sum / nBootstraps)
		val avgScoreBaseline = 100d * (baselineScores.sum / nBootstraps)
		val pVal = 1d - nBeatBaseline.toDouble / nBootstraps
		println("[virtualDevDataEvaluation] for %s, bootstrap score is %.2f, baseline is %.2f, p=%.3f".format(modelName, avgScore, avgScoreBaseline, pVal))
	}


	def getModelScoreDirs: Seq[ModelScoreDir] = {
		rootDir.listFiles
			.filter(_.isDirectory)
			.filter(_.getName.contains("_data=" + datasetName + "_"))
			.map(new ModelScoreDir(_))
	}


	/**
	 * given a model name like "parma+fert2", plots out a learning
	 * curve by looking for exeriments with "nTrain=\d+" in the name.
	 */
	def learningCurves(modelName: String) {

		def trainSize(model: ModelScoreDir): Int = {
			val p = """nTrain=(\d+)""".r
			p.findFirstMatchIn(model.fullName) match {
				case Some(m) =>
					m.group(1).toInt
				case None =>
					throw new RuntimeException("this has no nTrain param: " + model.fullName)
			}
		}

		val rel: Seq[ModelScoreDir] = getModelScoreDirs.filter(_.fullName.contains(modelName))
		for((model, rns) <- rel.groupBy(_.modelName)) {
			for((nTrain, runs) <- rns.groupBy(trainSize)) {
				val bestByDev = runs.maxBy(_.devScores.getOrElse("full-F1", 0d))
				log("%s\t%d\t%.2f".format(model, nTrain, bestByDev.testScores("full-F1") * 100d))
				//log(bestByDev.logFile.getPath)
				//log(grep(bestByDev.logFile, "limiting the number of training examples").mkString("\n"))
				//log(grep(bestByDev.logFile, "limiting").mkString("\n"))
			}
		}
	}


	/**
	 * saves a TSV for lemma, parma, and fert2 (referred to as quad)
	 * with one row for every doc alignment in EECB. columns indicate
	 * DA id, num possible alignments, F1
	 */
	def perfVsSize {

		// get the size of DAs by id
		log("building an index of the size of every DocAlignment in EECB...")
		val daSizes: Map[String, Int] = ConcreteDocAlignmentReader.EECB.getDocAlignments
			.map(da => (da.id, DocMetaAligner.numPossibleAlignments(da)))
			.toMap

		def toFile(description: String, scores: ModelScoreDir) {
			val f = new File("diagnostics/", description + ".size_vs_perf")
			log("writing out size vs perf to " + f.getPath)
			val w = FileUtils.getWriter(f)
			for(s <- scores.testInstanceScores) {
				val size: Int = daSizes(s.daId)
				val perf: Double = s.perf("full-F1") * 100d
				w.write("%s\t%d\t%.2f\n".format(s.daId, size, perf))
			}
			w.close
		}

		val all = getModelScoreDirs
		toFile("lemma", all.filter(_.fullName.startsWith("lemma")).head)
		toFile("parma", all.filter(_.fullName.startsWith("parma_")).head)
		toFile("quad", all.filter(_.fullName.startsWith("parma+fert2")).maxBy(_.devScores("full-F1")))
	}


	/** old way assumes there is dev data, otherwise will run virtualDevDataEvaluation */
	def compareAgainstBaseline(oldWay: Boolean) {
		// find the baseline's directory
		// find each systems directory
		// for every pair (baseline, other), call pValue

		val r = new Random(9001)
		
		println("computing results for " + datasetName + ":")
		val relevant: Seq[ModelScoreDir] = getModelScoreDirs

		// i accidentally ran parma with the other parameters (that don't affect it) swept.
		// each of these has the same performance, so i'll just take the first
		val baseline = relevant.filter(_.fullName.startsWith("parma_")).head
		log("baseline is " + baseline.modelName)

		val swsZero = ScoreWithSignificance(0d, 1d)
		val recomputeF1 = true	// by default F1 is macro average of document F1, which can lead to F1 < Precision && F1 < Recall
		val boldSignficantValues = true
		if(oldWay)
			println((" "*15) + " & F1 & P & R  &  Arg F1 & Arg P & Arg R  &  Pred F1 & Pred P & Pred R \\\\")
		val keys: Seq[String] = for(m <- Seq("full-", "pred-", "arg-"); e <- Seq("F1", "precision", "recall")) yield m + e
		for((modelName, runs) <- relevant.groupBy(_.modelName)) {
			if(oldWay) {
				//log("working on " + modelName)
				val (finished, errored) = runs.partition(_.isFinished)
				warnIf(errored.size > 0, "%d of %d runs failed for %s".format(errored.size, runs.size, modelName))
				val bestDev: ModelScoreDir = finished.maxBy(_.devScores.getOrElse("full-F1", 0d))
				val testResults: Map[String, ScoreWithSignificance] = pValue(bestDev, baseline)

				//println(bestDev.fullName + " ==============================")
				//println(testResults.map(kv => "\t%s = %s".format(kv._1, kv._2)).mkString("\n"))
				println("\n" + bestDev.fullName)

				val sb = new StringBuilder
				sb.append("%-15s".format(bestDev.modelName))
				for(k <- keys) {
					sb.append(" & ")
					val sws: ScoreWithSignificance = testResults.getOrElse(k, swsZero)
					if(boldSignficantValues && sws.pValue < 0.05d)
						sb.append("{\\bf")

					if(recomputeF1 && k.endsWith("F1")) {
						val m = k.split("-")(0)
						val p = testResults.getOrElse(m + "-precision", swsZero).score
						val r = testResults.getOrElse(m + "-recall", swsZero).score
						sb.append(" %.1f ".format(100d * 2d * p * r / (p + r)))
					}
					else sb.append(" %.1f ".format(sws.score * 100d))

					if(boldSignficantValues && sws.pValue < 0.05d)
						sb.append("}")
				}
				sb.append(" \\\\")
				println(sb.toString)

			}
			else virtualDevDataEvaluation(modelName, runs, baseline, r)
		}
	}

	private def shouldBeOne[T](lenOneSeq: Seq[T]): T = {
		require(lenOneSeq.size == 1, "lenOneSeq=" + lenOneSeq)
		lenOneSeq.head
	}
	
	/**
	 * returns a map from performance metric name to p-value of whether
	 *   perf(newSystem) > perf(baseline)
	 * computed by a bootstrapped CI of (perf(newSystem) - perf(baseline))
	 */
	private def pValue(newSystem: ModelScoreDir, baseline: ModelScoreDir,
			nBootstrapSamples: Int = 3000, r: Random = new Random(9001)): Map[String, ScoreWithSignificance] = {

		// zip together the Instance scores
		// for each performance metric:
		//   compute delta scores
		//   call the bootstrap CI code

		val nInstances: Map[String, InstanceScores] =
			newSystem.testInstanceScores.groupBy(_.daId)
				.mapValues(shouldBeOne)
		val bInstances: Map[String, InstanceScores] =
			baseline.testInstanceScores.groupBy(_.daId)
				.mapValues(shouldBeOne)

		require(nInstances.size == bInstances.size,
			"nInstances.size=%d, bInstances.size=%d".format(nInstances.size, bInstances.size) +
			" newSystem.dir= " + newSystem.dir.getPath)

		// run the bootstrap estimate for every performance metric
		val daIds: IndexedSeq[String] = nInstances.keys.toIndexedSeq
		val bootstraps: Map[String, MutablePairedBootstrap] = nInstances.values.head.perf.keys.map(m => (m, new MutablePairedBootstrap)).toMap
		for(daId <- daIds) {
			val nInst = nInstances(daId)
			val bInst = bInstances(daId)
			val deltaPerf: Map[String, Double] = Util.|-|(nInst.perf, bInst.perf)	// i thought we could do infix ops...
			for((metric, diff) <- deltaPerf)
				bootstraps(metric).accum(diff)
		}
		val seed = r.nextInt
		val pValues: Map[String, Double] = bootstraps.mapValues(_.pValue(nBootstrapSamples, new java.util.Random(seed)))

		// take the p-values and zip them up with the aggregate performance metrics for the new system
		pValues.keys.map(k => (k, ScoreWithSignificance(newSystem.testScores(k), pValues(k)))).toMap
	}

}

/** in code above, pValue is for this score vs a baseline */
case class ScoreWithSignificance(val score: Double, val pValue: Double) {
	override def toString: String = "%.3f (p=%.3f)".format(score, pValue)
}

/** scores for one DocAlignment */
case class InstanceScores(val daId: String, val perf: Map[String, Double])

/**
 * given a particular (model, configuration) that is housed in the given dir,
 * this extract and compute scores from log files
 */
class ModelScoreDir(val dir: File) {

	require(dir.isDirectory)

	lazy val logFile: File = {
		val f = new File(dir, "log.txt")
		require(f.isFile)
		f
	}

	/** name of model, e.g. "parma+psa" */
	lazy val fullName: String = {
		dir.getName.replace("Ablation-model=", "")
	}

	/** fullName but without parameter settings */
	lazy val modelName: String = {
		val i = fullName.indexOf("_")
		fullName.substring(0, i)
		//fullName
		//	.replaceFirst("_svmC=.+?_", "_")
		//	.replaceFirst("_svmD=.+?_", "_")
	}

	/** scores for every doc alignment in the corpus, evaluated when it was in the test set */
	lazy val testInstanceScores: Seq[InstanceScores] =
		extractScores(dir.list.filter(_.contains("subset=test")))

	/** scores for every doc alignment in the corpus, evaluated when it was in the dev set */
	lazy val devInstanceScores: Seq[InstanceScores] =
		extractScores(dir.list.filter(_.contains("subset=dev")))

	// aggregate (macro averaged) scores
	lazy val testScores: Map[String, Double] = aggregateScores(testInstanceScores)
	lazy val devScores: Map[String, Double] = aggregateScores(devInstanceScores)

	// check that this run actually finished
	lazy val isFinished: Boolean =
		Util.grep(logFile, "average on Ablation").length >= 3

	private def aggregateScores(instScores: Seq[InstanceScores]): Map[String, Double] = {
		import Util._
		if(instScores.size == 0)
			Map()
		else {
			val n = instScores.size.toDouble
			instScores.map(_.perf).reduce(|+|).mapValues(_ / n)
		}
	}

	private def extractScores(instanceScoreFileNames: Seq[String]): Seq[InstanceScores] = {
		/** @see ScoreLogger.logScores */
		def line2InstanceScores(line: String): InstanceScores = {
			val ar = line.split("\t")
			val keys =
				for(t <- Seq("full", "pred", "arg");
					m <- Seq("precision", "recall", "F1"))
					yield t + "-" + m
			val daId = ar(0)
			val perfs: Map[String, Double] = keys.zip(ar.drop(1).map(_.toDouble)).toMap
			InstanceScores(daId, perfs)
		}
		val r = instanceScoreFileNames
			.map(n => new File(dir, n))
			.flatMap(f => Source.fromFile(f).getLines)
			.map(line2InstanceScores)
			.toSeq

		// for RF dev, these will not be unique because some examples are persistently in the
		// dev set, which will count as duplicates when aggregated across folds
		//val uniq = r.groupBy(_.daId).size
		//require(uniq == r.size, "uniq=" + uniq + ", r.size=" + r.size + " for " + dir.getPath)
		r
	}
}

object Util {

	def pointwiseMapOp[T](m1: Map[T, Double], m2: Map[T, Double], op: (Double, Double) => Double): Map[T, Double] = {
		require(m1.size == m2.size)
		m1.keys.map(k => (k, op(m1(k), m2(k)))).toMap
	}

	// given two maps with the same keys, add their values
	def |+|(m1: Map[String, Double], m2: Map[String, Double]): Map[String, Double] = {
		pointwiseMapOp(m1, m2, (x,y) => x + y)
	}

	def |-|(m1: Map[String, Double], m2: Map[String, Double]): Map[String, Double] = {
		pointwiseMapOp(m1, m2, (x,y) => x - y)
	}

	def grep(f: File, simplePattern: String): Array[String] = {
		import sys.process._
		//println("searching through " + f.getPath)
		val cmd = Seq("grep", simplePattern, f.getPath)
		try {
			val results: String = cmd.!!
			results.split("\n")
		} catch {
			case e: Exception => Array()
		}
	}

}


