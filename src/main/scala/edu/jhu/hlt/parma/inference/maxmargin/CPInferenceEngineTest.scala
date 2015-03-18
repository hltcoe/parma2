package edu.jhu.hlt.parma.inference.maxmargin

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.input._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.util.Color._
import edu.jhu.hlt.parma.experiments._
import edu.jhu.hlt.parma.inference._
import edu.jhu.hlt.parma.evaluation._
import edu.jhu.hlt.parma.inference.maxmargin._
import java.io.{ File, PrintStream }
import java.util.Random
import collection.mutable.ArrayBuffer

object TestData {
	val logDir = new File("diagnostics/ssvm-tests")
	val rand = new Random(9001)

	lazy val das = getDAs(2000, notTooBig _)
	DocAlignmentCorpus.rand.setSeed(9002)
	lazy val corpus = DocAlignmentCorpus.randomSplit("eecb+rf", das, 0.6d, 0.2d)

	val maxAlignmentGridSize = ParmaConfig.getInt("tests.cptest.max-alignments", 500)
	def notTooBig(da: DocAlignment): Boolean = {
		DocMetaAligner.allPossiblePredAlignments(da.context).size < maxAlignmentGridSize &&
			DocMetaAligner.allPossibleArgCorefAlignments(da.context).size < maxAlignmentGridSize
	}

	def getDAs(howMany: Int, predicate: DocAlignment => Boolean): IndexedSeq[DocAlignment] = {
		/*
		val local = new File("/state/partition1/travis")
		if(!local.isDirectory) local.mkdir
		val localF = new File(local, "eecb.das.pb")
		if(!localF.isFile) {
			import sys.process._
			val cmd = "cp %s %s".format(
				ParmaConfig.getFile("data.concrete.eecb.alignments").getPath,
				localF.getPath)
			val r = cmd.!
			require(r == 0)
		}
		*/
		val dar = ConcreteDocAlignmentReader.EECB
		dar.daIter.filter(predicate).take(howMany).toIndexedSeq
	}
}

/**
 * learn some weights, measure your performance.
 * then add noise to your weights and your performance should go down.
 */
object Basic extends CPTest {
	override def run {
		// v1 = objective with random weights
		// v2 = objective after 1 step of optimization
		// assert v1 > v2
		ssvm.redirectLogTo(workingDir.uniqFile(flags=Seq("ssvm-train")))
		ssvm.train(trainData.map(ssvm.computeFeatures), maxIter=50)	// first, optimize so that a few constraints get added
		logBestParameters(ssvm)
		val v1 = ssvm.objectiveFunction.getValue
		for(i <- 1 to 20) {
			log("setting theta to a random gaussian vector")
			ssvm.theta = DVec.gaussian(ssvm.theta.dimension, 1e-1, rand)
			import scala.language.reflectiveCalls
			ssvm.objectiveFunction.setDirty
			val v2 = ssvm.objectiveFunction.getValue
			
			log("v1=%.8g v2=%.8g".format(v1, v2))
			assert(v1 > v2)
		}
	}
}

/**
 * SSVM should be a proper model superset of HAM, so it
 * should out-perform it. If not, check parameters
 */
object CompareToHam extends CPTest {
	val size = 200
	override def run {
		log("training ham...")
		val ham = new HierarchicalAlignmentModule
		trainAndEval(ham, size)

		log("training ssvm...")
		val ie = new CPInferenceEngineFast
		ie.redirectLogTo(workingDir.uniqFile(flags=Seq("ssvm-train")))
		trainAndEval(ie, size)
	}
}

/**
 * plot out the runtime vs the size of the constraint caches that
 * every alignment keeps
 */
object EffectOfConstraintCaching extends CPTest {
	override def run {
		for(cacheSize <- Seq(1, 5, 25, 125, 625)) {
			log("training with cache size " + cacheSize)
			ssvm.redirectLogTo(workingDir.uniqFile(props=Map("cacheSize"->cacheSize.toString)))
			ssvm.constraintCacheSize = cacheSize
			val time = Profiler.getTime {
				ssvm.train(trainData.map(ssvm.computeFeatures), useConstraintCaching=true)
			}
			log("cacheSize=%d perf=%.4f time=%.1f".format(cacheSize, evaluate(ssvm), time))
			log("" + ssvm.cacheStats)
			logBestParameters(ssvm)
		}
	}
}

/**
 * when you train on more examples, your test set performance should
 * (stochastically) increase
 */
object PerformanceAlwaysIncreases extends CPTest {
	override def run {
		
		// ham for comparison (shouldn't take long...)
		val ham = new HierarchicalAlignmentModule
		log("training ham...")
		ham.redirectLogTo(workingDir.uniqFile(flags=Seq("ham-train")))
		val data = TestData.corpus.train.take(200)
		ham.preTrainCalibrate(data)
		val hamTrainTime = Profiler.getTime {
			ham.train(data)
		}
		ham.postTrainCalibrate(TestData.corpus.dev, loss)
		evaluate(ham, Some(workingDir.uniqFile(flags=Seq("ham-predictions"))))
		logBestParameters(ham)


		var prevPerf = 0d
		val beatPrev = new Sample
		for(size <- Seq(6, 12, 24, 48, 96, 192, 99999)) {

			val s = math.min(size, TestData.corpus.train.size)
			log("training on up to %d alignments".format(s))

			val ie = new CPInferenceEngineFast
			ie.redirectLogTo(workingDir.uniqFile(props=Map("numTrain"->s.toString)))

			ie.setQuadraticFertilityCosts
			ie.disablePSAFactors
			ie.disableASPFactors
			ie.tempOrdMethod = NoTemporalFactors

			trainAndEval(ie, size)
		}
		// should at least get better when you increase the
		// train size, on average
		log("\navg increase = %.3f".format(beatPrev.mean))
		assertTrue(beatPrev.mean > 0d)
	}
}

/**
 * Vary falseNegPenalty. As it gets bigger, recall should increase.
 * Find a good tradeoff. My guess would be ~2.
 */
object EffectOfFalseNegPenalty extends CPTest {
	override def run {
		val n = 200
		for(falseNegBias <- Seq(1d, 2d, 3d, 4d, 5d)) {
			val ie = new CPInferenceEngineFast
			ie.redirectLogTo(workingDir.uniqFile(props=Map("falseNegBias"->falseNegBias.toString)))
			ie.falseNegBias = falseNegBias
			log("starting falseNegBias = " + falseNegBias)
			trainAndEval(ie, maxExamples=n)
		}
	}
}


/**
 * just a sweep of svmC to make sure we get a good value
 */
object EffectOfMacroSmoothing extends CPTest {
	override def run {
		val n = 200
		for(smoothing <- Seq(0d, 1d, 10d, 100d, 1000d)) {
			System.setProperty("inference.ssvm.macro-constraint-weighting", smoothing.toString)
			log("starting smoothing=%f".format(smoothing))
			val ie = new CPInferenceEngineFast
			ie.redirectLogTo(workingDir.uniqFile(props=Map("smoothing"->smoothing.toString)))
			ie.tempOrdMethod = NoTemporalFactors
			trainAndEval(ie, maxExamples=n)
		}
	}
}

/**
 * see how the parameter which trades off for optimizing macro vs micro F1
 * affects overall performance
 */
object EffectOfSvmC extends CPTest {
	override def run {
		val n = 200
		val svmC0 = ParmaConfig.getDouble("inference.ssvm.svm-c")
		for(svmC <- Seq(1d, 0.3d, 3d, 0.1d, 10d, 0.05d, 20d).map(_ * svmC0)) {
			log("starting svmC=%.8g".format(svmC))
			ssvm.svmC = svmC
			ssvm.redirectLogTo(workingDir.uniqFile(props=Map("svmC"->svmC.toString)))
			trainAndEval(ssvm, maxExamples=n)
		}
	}
}

/**
 * I don't think this will make a big differnce... check to make sure
 * @deprecated no longer needed when we use cplex for primal optimization
 */
object EffectOfQPOptIter extends CPTest {
	override def run {
		for(iter <- Seq(10, 100, 1000)) {
			log("starting iter=" + iter)
			ssvm.redirectLogTo(workingDir.uniqFile(props=Map("qpIter"->iter.toString)))
			val time = Profiler.getTime {
				ssvm.train(trainData.map(ssvm.computeFeatures), qpOptIter=iter)
			}
			log("qpOptIter=%d perf=%.4f time=%.1f".format(iter, evaluate(ssvm), time))
			logBestParameters(ssvm)
		}
	}
}

/**
 * @deprecated no longer needed when we use cplex for primal optimization
 */
object EffectOfLearningRate extends CPTest {
	override def run {
		for(learningRate <- Seq(1e-1, 1e0, 1e-2, 1e1, 1e-3)) {//, 1e-4)) {
			log("learning with learningRate=%.1g".format(learningRate))
			ssvm.learningRate = learningRate
			ssvm.redirectLogTo(workingDir.uniqFile(props=Map("learningRate"->learningRate.toString)))
			val time = Profiler.getTime {
				ssvm.train(trainData.map(ssvm.computeFeatures))
			}
			log("learningRate=%.1g perf=%.4f time=%.1f".format(learningRate, evaluate(ssvm), time))
			logBestParameters(ssvm)
		}
	}
}

class EffectOfQuadraticCosts(val doAll: Boolean) extends CPTest {
	override def name: String =
		"EffectOfQuadraticCosts" + (if(doAll) "Full" else "Partial")
	override def run {
		// ham
		val ham = new HierarchicalAlignmentModule
		ham.redirectLogTo(workingDir.uniqFile(flags=Seq("ham")))
		trainAndEval(ham)


		if(doAll) {
			// ssvm fert0 (should roughly match ham)
			val f0 = new CPInferenceEngineFast
			f0.redirectLogTo(workingDir.uniqFile(flags=Seq("fert0")))
			f0.disableFertilityCosts
			f0.disablePSAFactors
			trainAndEval(f0)

			// ssvm fert1
			val f1 = new CPInferenceEngineFast
			f1.redirectLogTo(workingDir.uniqFile(flags=Seq("fert1")))
			f1.setFirstOrderFertilityCosts
			f1.disablePSAFactors
			trainAndEval(f1)
		}

		// ssvm fert2
		val f2 = new CPInferenceEngineFast
		f2.redirectLogTo(workingDir.uniqFile(flags=Seq("fert2")))
		f2.setQuadraticFertilityCosts
		f2.disablePSAFactors
		trainAndEval(f2)

		if(doAll) {
			// ssvm PSA
			val s = new CPInferenceEngineFast
			s.redirectLogTo(workingDir.uniqFile(flags=Seq("share")))
			s.disableFertilityCosts
			s.enablePSAFactors
			trainAndEval(s)
		}

		// ssvm PSA + fert2
		val sf2 = new CPInferenceEngineFast
		sf2.redirectLogTo(workingDir.uniqFile(flags=Seq("share", "fert2")))
		sf2.setQuadraticFertilityCosts
		sf2.enablePSAFactors
		trainAndEval(sf2)

		// NOTE:
		// missing ASP and temporal, checkout experiments.acl2014.Ablation
	}
}

/**
 * one full train + test run with full model
 */
object Full extends CPTest {
	override def run {
		// ssvm preds-share-args + fert=2
		val n = ParmaConfig.getInt("tests.cptest.cplex-qp-numTrain", 20)
		val time = Profiler.getTime {
			val ie = new CPInferenceEngineFast
			ie.redirectLogTo(workingDir.uniqFile(flags=Seq("full")))
			ie.setQuadraticFertilityCosts
			ie.splitPredArgFertWeights = true
			trainAndEval(ie, maxExamples=n)
		}
		log("[Full] took %.1f minutes".format(time / 60d))
	}
}

object SplitFertCosts extends CPTest {
	override def run {
		val splitTime = Profiler.getTime {
			ssvm.redirectLogTo(workingDir.uniqFile(flags=Seq("split")))
			ssvm.setQuadraticFertilityCosts
			ssvm.splitPredArgFertWeights = true
			trainAndEval(ssvm)
		}
		val noSplitTime = Profiler.getTime {
			ssvm.redirectLogTo(workingDir.uniqFile(flags=Seq("nosplit")))
			ssvm.setQuadraticFertilityCosts
			ssvm.splitPredArgFertWeights = false
			trainAndEval(ssvm)
		}
		log("[SplitFertCosts] split took %.1f minutes, nosplit took %.1f".format(splitTime / 60d, noSplitTime / 60d))
	}
}

object CplexPrimal extends CPTest {
	override def run {
		val n = 150	// how many examples to do training on
		val cplexTime = Profiler.getTime {
			val ie = new CPInferenceEngineFast
			ie.redirectLogTo(workingDir.uniqFile(flags=Seq("cplex")))
			ie.setQuadraticFertilityCosts
			ie.splitPredArgFertWeights = false
			trainAndEval(ie, maxExamples=n)
		}
		val malletTime = Profiler.getTime {
			val ie = new CPInferenceEngine
			ie.redirectLogTo(workingDir.uniqFile(flags=Seq("mallet")))
			ie.setQuadraticFertilityCosts
			ie.splitPredArgFertWeights = false
			trainAndEval(ie, maxExamples=n)
		}
		log("[CplexPrimal] for %d examples: cplex took %.1f minutes, mallet took %.1f"
			.format(n, cplexTime / 60d, malletTime / 60d))
	}
}

class CplexQPParams(val svmC: Double, val lossMult: Double) extends CPTest {
	override def name: String =
		"CplexQPParams_svmC%f_lossMult%f_useFert=%s_useShare=%s"
			.format(svmC, lossMult,
				ParmaConfig.getBoolean("tests.cptest.cplex-qp-useFert"),
				ParmaConfig.getBoolean("tests.cptest.cplex-qp-useShare"))
	override def run {
		val n = ParmaConfig.getInt("tests.cptest.cplex-qp-numTrain", 20)	// how many examples to do training on
		val cplexTime = Profiler.getTime {
			val ie = new CPInferenceEngineFast
			ie.redirectLogTo(workingDir.uniqFile(props=Map("svmC"->svmC.toString, "lossMult"->lossMult.toString)))
			ie.svmC = svmC
			ie.lossMultiplier = lossMult
			trainAndEval(ie, maxExamples=n)
		}
		log("svmC=%f lossMult=%f time=%.1f min".format(svmC, lossMult, cplexTime / 60d))
	}
}

object CplexPrimalParams extends CPTest {
	override def run {
		val n = 200	// how many examples to do training on
		val splitFertCosts = false

		val cplexTime = Profiler.getTime {
			val ie = new CPInferenceEngineFast
			ie.redirectLogTo(workingDir.uniqFile(flags=Seq("vanilla")))
			ie.setQuadraticFertilityCosts
			ie.splitPredArgFertWeights = splitFertCosts
			trainAndEval(ie, maxExamples=n)
		}
		log("vanilla time=%.1f min".format(cplexTime / 60d))

		val c0 = ParmaConfig.getDouble("inference.ssvm.svm-c")
		val d0 = ParmaConfig.getDouble("inference.ssvm.loss-mult")
		for(c <- Seq(c0, c0/3d, c0*3d, c0/10d, c0*10d)) {
			for(d <- Seq(d0, d0/3d, d0*3d, d0/10d, d0*10d)) {
				val cplexTime = Profiler.getTime {
					val ie = new CPInferenceEngineFast
					ie.redirectLogTo(workingDir.uniqFile(props=Map("c"->c.toString, "d"->d.toString)))
					ie.svmC = c
					ie.lossMultiplier = d
					ie.setQuadraticFertilityCosts
					ie.splitPredArgFertWeights = splitFertCosts
					trainAndEval(ie, maxExamples=n)
				}
				log("c=%f d=%f time=%.1f min".format(c, d, cplexTime / 60d))
			}
		}

	}
}

class SynTempOrdExperiment(val info: Double) extends CPTest {
	override def name: String = "SynTempOrdExperiment_info=%f".format(info)
	override def run {
		
		val n = 200	// how many examples to do training on

		val tsTime = Profiler.getTime {
			val ie = new CPInferenceEngineFast
			ie.redirectLogTo(workingDir.uniqFile(flags=Seq("timesieve")))
			ie.tempOrdMethod = TimeSieveTemporalFactors
			trainAndEval(ie, maxExamples=n)
		}
		log("time-sieve time=%.1f min".format(tsTime / 60d))

		val time = Profiler.getTime {
			val ie = new CPInferenceEngineFast
			ie.redirectLogTo(workingDir.uniqFile(flags=Seq("fakeTempOrd"), props=Map("information"->info.toString)))
			ie.tempOrdMethod = FakeTemporalFactors(info)
			trainAndEval(ie, maxExamples=n)
		}
		log("info=%.1f time=%.1f min".format(info, time / 60d))

		val noTempTime = Profiler.getTime {
			val ie = new CPInferenceEngineFast
			ie.redirectLogTo(workingDir.uniqFile(flags=Seq("noTempOrd")))
			ie.tempOrdMethod = NoTemporalFactors
			trainAndEval(ie, maxExamples=n)
		}
		log("noTemp time=%.1f min".format(noTempTime / 60d))

	}
}

trait CPTest extends Logging2 {

	private[this] val base = ParmaConfig.getDirectory("diagnostics.ssvm-test-dir")
	val archive = new WorkingDirectory(new File(base, "archive"))
	val workingDir = new WorkingDirectory(new File(base, name))
	workingDir.redirectLogTo(this)
	val ssvm = new CPInferenceEngineFast
	val rand = new Random(9001)

	def trainData: Seq[DocAlignment] = TestData.corpus.train
	def devData: Seq[DocAlignment] = TestData.corpus.dev

	def setup {
		workingDir.archiveContentsTo(
			archive.uniqFile(flags=Seq(workingDir.home.getName), suffix=".zip"))

		redirectLogTo(workingDir.uniqFile(flags=Seq("top")))
		ssvm.preTrainCalibrate(trainData)

		log(Describe.memoryUsage())
	}

	def name: String = getClass.getName.split("\\.").last.replace("$", "")

	/** you implement this */
	def run()

	def cleanup {
		Profiler.writeoutTimes(getLogger.getOutputStream)
		Profiler.clearStartTimes
		flushLog
	}

	def logBestParameters(ie: InferenceEngine[_], k: Int = 30) {
		log("%d best parameters for %s:".format(k, ie.name))
		for((name, idx, weight) <- ie.biggestParameters(k))
			log("%.80s %.3g".format(name, weight))
		if(ie.isInstanceOf[CPInferenceEngine]) {
			val cp = ie.asInstanceOf[CPInferenceEngine]
			log("==== special params ====")
			for(p <- cp.quadraticParams)
				log("%.80s %.4g".format(p.name, p.value))
		}
	}

	def loss: EvaluationLoss = MacroF1.asLossFunction

	def trainAndEval[T <: FeatureRepresentation](ie: InferenceEngine[T], maxExamples: Int = 9999999) {
		ie.preTrainCalibrate(trainData)

		import edu.jhu.hlt.parma.util.Reservoir.Sample
		implicit val rand = new Random(9001)	// fix the sample of the training set
		val time = Profiler.getTime { ie.train(trainData.reservoir(maxExamples)(rand)) }
		log("[trainAndEval] training took %.1f seconds".format(time))
		ie.log("[trainAndEval] training took %.1f seconds".format(time))

		ie.postTrainCalibrate(devData, loss)

		val (evalTime, perf) = Profiler.getTimeAndValue { evaluate(ie) }
		val logTime = Profiler.getTime { logBestParameters(ie) }
		log("[trainAndEval] evaluation took %.1f seconds, logging best params took %.1f".format(evalTime, logTime))
		ie.log("[trainAndEval] evaluation took %.1f seconds, logging best params took %.1f".format(evalTime, logTime))
	}

	def evaluate[T <: FeatureRepresentation](ie: InferenceEngine[T], writeoutInstancesTo: Option[File] = None): Double = {

		val start = System.currentTimeMillis
		val threads = ParmaConfig.getInt("inference.ssvm.decode-threads", 1)

		val frs: Seq[T] = TestData.corpus.test.map(da => ie.computeFeatures(da.report, da.passage, da.domain))
		val hyps: Seq[DocAlignment] = frs.map(ie.align)
		val golds: Seq[DocAlignment] = TestData.corpus.test
		val instances = for((h,g) <- hyps.zip(golds)) yield Instance(h, g)

		/*
		val dawfs: Seq[DocAlignmentWithFeatures[T]] = TestData.corpus.test.map(ie.computeFeatures)
		val hyps: Seq[DocAlignment] = 
			if(threads > 1 && ie.isInstanceOf[CPInferenceEngine])
				ie.asInstanceOf[CPInferenceEngine].parallelAlign(dawfs.map(_.features).asInstanceOf[Seq[CPFeatureRepr]], threads)
			else dawfs.map((dawf: DocAlignmentWithFeatures[T]) => ie.align(dawf.features))
		val instances =
			for((hypDA, dawf) <- hyps.zip(dawfs))
				yield Instance(hypDA, dawf.alignment)
		*/

		import edu.jhu.hlt.parma.diagnostics.FeatureDiagnostics
		import java.io.PrintWriter
		writeoutInstancesTo match {
			case None => {}
			case Some(f) =>
				log("writing alignments out to " + f.getPath)
				require(!f.isDirectory)
				val w = new PrintWriter(FileUtils.getWriter(f))
				for((inst, fr) <- instances.zip(frs))
					FeatureDiagnostics.writeoutAlignmentFeatures(inst, fr, w)
				w.close
		}

		// use hamming for now, that is what ssvm actually optimizes
		def avg(sd: Seq[Double]) = sd.sum / sd.size
		//val h = avg(instances.map(i => 1d - SetBasedEvaluator.hamming(i, normalize=true)))
		//val f1 = avg(instances.map(i => SetBasedEvaluator.generousF1(i)))
		val h = 1d - MacroHamming(instances)
		val f1 = MicroF1(instances)
		log("[evaluate] 1-hamming=%.4f f1=%.4f time=%.1f seconds on %d instances"
			.format(h, f1, (System.currentTimeMillis-start)/1000d, hyps.size))

		h
	}

	// TODO replace this with JUnit's version when you get sbt testing working
	def assertEquals(expected: Any, observed: Any) {
		if(expected != observed)
			throw new RuntimeException("expected %s, observed %s".format(expected, observed))
	}
	def assertTrue(b: Boolean) { if(!b) throw new RuntimeException }
}

object CPTestRunner {

	lazy val tests = Map(
		"Full" -> Full,		// one full train + test run with full model
		"Basic" -> Basic,
		"CompareToHam" -> CompareToHam,
		"SplitFertCosts" -> SplitFertCosts,
		"CplexPrimal" -> CplexPrimal,
		"CplexPrimalParams" -> CplexPrimalParams,
		"PerformanceAlwaysIncreases" -> PerformanceAlwaysIncreases,
		"EffectOfQuadraticCosts" -> new EffectOfQuadraticCosts(false),
		"EffectOfQuadraticCostsPartial" -> new EffectOfQuadraticCosts(false),
		"EffectOfQuadraticCostsFull" -> new EffectOfQuadraticCosts(true),
		"EffectOfConstraintCaching" -> EffectOfConstraintCaching,
		"EffectOfFalseNegPenalty" -> EffectOfFalseNegPenalty,
		"EffectOfSvmC" -> EffectOfSvmC,
		"EffectOfMacroSmoothing" -> EffectOfMacroSmoothing,
		"EffectOfLearningRate" -> EffectOfLearningRate,
		"EffectOfQPOptIter" -> EffectOfQPOptIter)

	def main(args: Array[String]) {
		if(args.length < 2) {
			println("please provide:")
			println("1) a parma.config file")
			println("2) one or more tests to run")
			return
		}
		ParmaConfig.load(args(0))
		for(tname <- args.drop(1)) {
			if(tname == "CplexQPParams") {	// oh man this is ugly...
				runTest(new CplexQPParams(
					ParmaConfig.getDouble("inference.ssvm.svm-c"),
					ParmaConfig.getDouble("inference.ssvm.loss-mult")))
			} else if(tname == "SynTempOrdExperiment") {
				runTest(new SynTempOrdExperiment(
					ParmaConfig.getDouble("tests.cptest.fake-temp-info")))
			} else {
				tests.get(tname) match {
					case None =>
						println("there is no test with name \"%s\", skipping".format(tname))
					case Some(f) =>
						runTest(f)
				}
			}
		}
	}

	def runTest(f: CPTest) {
		f.setup
		try {
			val time = Profiler.getTime { f.run }
			f.log("[CPTestRunner] run took %.1f seconds".format(time))
		} catch {
			case t: Throwable =>
				t.printStackTrace(new PrintStream(f.getLogger.getOutputStream))
		}
		f.cleanup
	}
}

object MalletLogging {
	def shutUp {
		import cc.mallet.optimize._
		import cc.mallet.util._
		import java.util.logging.{ Filter, LogRecord, Level }
		val noopFilter = new Filter { override def isLoggable(r: LogRecord): Boolean = false }
		for(l <- Seq(
			MalletLogger.getLogger(classOf[GradientAscent].getName),
			MalletLogger.getLogger(classOf[LimitedMemoryBFGS].getName),
			MalletLogger.getLogger(classOf[BackTrackLineSearch].getName))) {

			// try to kill it in 3 ways!
			l.setFilter(noopFilter)
			l.setLevel(Level.SEVERE)
			for(h <- l.getHandlers)
				l.removeHandler(h)
		}

	}
}


