package edu.jhu.hlt.parma.inference.maxmargin
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.inference._
import edu.jhu.hlt.parma.evaluation._
import cc.mallet.optimize._
import ilog.concert._
import ilog.cplex._
import java.util.Arrays
import collection.mutable.ArrayBuffer
import scala.language.reflectiveCalls
import util.Random
import java.util.concurrent._
import java.io.File

case class LPBenchmark(val da: DocAlignment, val size: Int, val ilpTime: Double, val lpTime: Double,
		val deltaF1: Double,	// F1 between the exact and relaxed decodes
		val exactF1: Double,	// F1 score of exact decoding
		val relaxF1: Double) {	// F1 score of linear relaxed decoding
	def speedup: Double = ilpTime / lpTime
	def slow: Boolean = ilpTime > 2d
}

class CPFeatureRepr(
		val cpAlignment: CPGlobalAlignment,
		override val controller: CPInferenceEngine)
		extends FeatureRepresentation {
	def inspectFeatures: Option[scala.collection.Map[Alignment, SVec]] =
		Some((cpAlignment.preds ++ cpAlignment.args).map(a => (a.alignment, a.features)).toMap)
	def inspectScores: Option[scala.collection.Map[Alignment, Double]] =
		Some(inspectFeatures.get.mapValues(sv => VecOps.dot(controller.theta, sv)))
	def report: Document = cpAlignment.report
	def passage: Document = cpAlignment.passage
	/**
	 * NOTE: be careful with this!
	 * if you want to sort the alignments tasks by difficulty, you need to store
	 * the permutation matrix so you can return the alignments in the correct order
	 */
	def estimatedDecodeDifficulty: Double =
		DocMetaAligner.numPossibleAlignments(report, passage).toDouble
}

sealed abstract class TemporalMethod
object NoTemporalFactors extends TemporalMethod
case class FakeTemporalFactors(val information: Double) extends TemporalMethod { require(information >= 0d) }
object TimeSieveTemporalFactors extends TemporalMethod

class CPInferenceEngine extends InferenceEngine[CPFeatureRepr] {

	var decodeScoreOffset = 0d	// effectively an extra unary factor on every alignment tuned to minimize loss on dev data
	var svmC = ParmaConfig.getDouble("inference.ssvm.svm-c")
	var splitPredArgFertWeights = ParmaConfig.getBoolean("inference.ssvm.split-fert-costs")
	var includeRowSizeFeatures = ParmaConfig.getBoolean("inference.ssvm.use-fert-row-size-features")
	var lossMultiplier = ParmaConfig.getDouble("inference.ssvm.loss-mult")
	var falseNegBias: Double = ParmaConfig.getDouble("inference.ssvm.false-neg-bias", 1d)
	var lpRelaxedTraining = ParmaConfig.getBoolean("inference.ssvm.lpRelaxedTraining", true)
	var lpRelaxedDecoding = ParmaConfig.getBoolean("inference.ssvm.lpRelaxedDecoding", true)

	// not used in CPInferenceEngineFast
	var learningRate = 1e-2 / math.pow(1d + svmC, 0.75)
	var useLBFGS = true	// if true, uses LBFGS+GradientDescent, else uses just GradientDescent for QP optimization

	var constraintCacheSize = CPGlobalAlignment.defaultMVCacheSize	// how many possible alignments/constraints to cache per alignment
	var hasPretrained = false
	val workingSet = new WorkingSet(checkSignatures=false)
	val cacheStats = new CacheStats("1-Slack MV")

	// setup up logging
	workingSet.redirectLogTo(this)
	cacheStats.redirectLogTo(this)

	// parameters
	// TODO need to move to FeatureSet
	// TODO count needed dimensions
	val ham = new HierarchicalAlignmentModule	// TODO replace with FeatureSet
	var theta = ham.parametersNoCopy
	val alph = ham.alph

	// if pred/args share the same fert parameters, we use fert1Preds and fert2Preds
	val fert1 = new AlphParamRefRange(alph,
		IndexedSeq("fert1-centroid", "fert1-preds", "fert1-args",
		"fert1-size-a", "fert1-size-b", "fert1-size-c", "fert1-size-d",
		"fert1-size-e", "fert1-size-f", "fert1-size-g"), theta) with Activatable
	val fert2 = new AlphParamRefRange(alph,
		IndexedSeq("fert2-centroid", "fert2-preds", "fert2-args",
		"fert2-size-a", "fert2-size-b", "fert2-size-c", "fert2-size-d",
		"fert2-size-e", "fert2-size-f", "fert2-size-g"), theta) with Activatable

	// preds share args
	val psa11 = new FixedParamRef("preds-share-args-11", alph, theta) with Activatable
	val psa10 = new FixedParamRef("preds-share-args-10", alph, theta) with Activatable
	val psa01 = new FixedParamRef("preds-share-args-01", alph, theta) with Activatable

	// args share preds
	val asp11 = new FixedParamRef("args-share-preds-11", alph, theta) with Activatable
	val asp10 = new FixedParamRef("args-share-preds-10", alph, theta) with Activatable
	val asp01 = new FixedParamRef("args-share-preds-01", alph, theta) with Activatable

	val tempOrd = new FixedParamRef("temporal-ordering", alph, theta) with Activatable
	var tempOrdMethod: TemporalMethod = NoTemporalFactors

	// new
	val psaNewNew = new AlphParamRefRange(alph, IndexedSeq("psa-nn-centroid", "psa-nn-a", "psa-nn-b", "psa-nn-c"), theta) with Activatable
	val psaNewOld = new AlphParamRefRange(alph, IndexedSeq("psa-no-centroid", "psa-no-a", "psa-no-b", "psa-no-c"), theta) with Activatable
	val aspNewNew = new AlphParamRefRange(alph, IndexedSeq("asp-nn-centroid", "asp-nn-a", "asp-nn-b", "asp-nn-c"), theta) with Activatable
	val aspNewOld = new AlphParamRefRange(alph, IndexedSeq("asp-no-centroid", "asp-no-a", "asp-no-b", "asp-no-c"), theta) with Activatable
	val useNewSharing = ParmaConfig.getBoolean("use-new-sharing")

	var readParamsInsteadOfTraining: Option[File] = None

	implicit val rand = new Random(9001)
	
	def quadraticParams = fert1.paramRefs ++ fert2.paramRefs ++ Seq(psa10, psa01, psa11, asp10, asp01, asp11, tempOrd) ++
		psaNewNew.paramRefs ++ psaNewOld.paramRefs ++ aspNewNew.paramRefs ++ aspNewOld.paramRefs

	def disableFertilityCosts {
		log("disabling fertility costs")
		fert1.deactivate
		fert2.deactivate
	}
	def setFirstOrderFertilityCosts {
		log("enabling 0-1 fertility costs")
		fert1.activate
		fert2.deactivate
	}
	def setQuadraticFertilityCosts {
		log("enabling quadratic fertility costs")
		fert1.activate
		fert2.activate
	}

	// when this is false, and we use 01 and 10 vars, things are screwy (e.g. performance doesn't always go up during training)
	val fastSharingFactors = ParmaConfig.getBoolean("inference.ssvm.fast-sharing-factors")

	def enablePSAFactors {
		log("enabling PSA")
		if(useNewSharing) {
			psaNewNew.activate
			psaNewOld.activate
		}
		else {
			if(fastSharingFactors) {
				psa10.deactivate
				psa01.deactivate
			} else {
				psa10.activate
				psa01.activate
			}
			psa11.activate
		}
	}
	def disablePSAFactors {
		log("disabling PSA")
		psaNewNew.deactivate
		psaNewOld.deactivate
		psa10.deactivate
		psa01.deactivate
		psa11.deactivate
	}

	def enableASPFactors {
		log("enabling ASP")
		if(useNewSharing) {
			aspNewNew.activate
			aspNewOld.activate
		}
		else {
			if(fastSharingFactors) {
				asp10.deactivate
				asp01.deactivate
			} else {
				asp10.activate
				asp01.activate
			}
			asp11.activate
		}
	}

	def disableASPFactors {
		log("disabling ASP")
		aspNewNew.deactivate
		aspNewOld.deactivate
		asp10.deactivate
		asp01.deactivate
		asp11.deactivate
	}

	// defaults
	disableASPFactors
	disablePSAFactors
	setQuadraticFertilityCosts

	var verbose = true
	var cpaVerbose = false

	/*
	 * assume for now that we get everything other than the weights from
	 * configuration that happens from above
	 *
	 * the ides should be that we can swap out training for a call to readParameters
	 * and the only thing missing is stuff we don't need (like featureName)
	 */

	override def readParameters(f: java.io.File) {
		log("[CPInferenceEngine readParameters] from " + f.getPath)
		val r = FileUtils.getReader(f)
		val lineOne = r.readLine
		require(lineOne == "CPInferenceEngine", "unknown class: " + lineOne)
		r.readLine	// train time
		r.readLine	// svmC
		r.readLine	// fnBias
		r.readLine	// warning
		val stage = theta.copy
		val n = theta.dimension
		var i = 0
		while(i < n) {
			val line = r.readLine
			//log("line(%d) = \"%s\"".format(i, line))
			stage(i) = line.toDouble
			i += 1
		}
		warnIf(r.ready, "did you match the dimension correctly?")
		r.close
		theta = stage
	}

	override def writeoutParameters(f: java.io.File) {
		log("[CPInferenceEngine writeoutParameters] to " + f.getPath)
		val w = FileUtils.getWriter(f)
		w.write("CPInferenceEngine\n")
		w.write("trained at " + new java.util.Date().toString + "\n")
		w.write("C=%f\n".format(svmC))
		w.write("falseNegBias=%f\n".format(falseNegBias))
		w.write("other parameters are not checked, make sure you got them right!\n")
		w.write(theta.getArray.mkString("\n") + "\n")
		w.close

		val ff = new File(f.getPath + ".bin")
		ModelIO.writeModel(ff, theta, alph, humanReadable=false)
	}

	protected def logParams(prefix: String, params: DVec)  {
		for(p <- quadraticParams)
			log("%s %s @ %d = %.6g".format(prefix, p.name, p.index, params(p.index)))
		log("%s L1=%.3f L2=%.3f Linf=%.3f".format(prefix, params.l1, params.l2, params.lInf))
	}

	override def computeFeatures(da: DocAlignment): DocAlignmentWithFeatures[CPFeatureRepr] = {
		def featureFunc(a: Alignment, c: Context): SVec =
			ham.computeFeatures(a, c.report, c.passage, da.domain)
		Profiler.startTask("cp:computeFeatures")
		val cpa = new CPGlobalAlignment(constraintCacheSize)
		cpa.verbose = verbose && cpaVerbose
		cpa.redirectLogTo(this)
		cpa.initData(featureFunc _, da, falseNegBias)
		cpa.addFertilityFactors(fert1, fert2, splitPredArgFertWeights, includeRowSizeFeatures)
		cpa.addSharingFactors(psaNewNew, psaNewOld, aspNewNew, aspNewOld, psa10, psa01, psa11, asp10, asp01, asp11)
		tempOrdMethod match {
			case FakeTemporalFactors(info) =>
				cpa.addFakeTemporalOrderingFactors(info, tempOrd)
			case TimeSieveTemporalFactors =>
				cpa.addTSTemporalOrderingFactors(tempOrd)
			case NoTemporalFactors => {}
		}
		Profiler.endTask("cp:computeFeatures")
		new DocAlignmentWithFeatures(da, new CPFeatureRepr(cpa, this))
	}

	override def computeFeatures(report: Document, passage: Document, domain: Option[String]): CPFeatureRepr = {
		def featureFunc(a: Alignment, c: Context) = ham.computeFeatures(a, c.report, c.passage, domain)
		Profiler.startTask("cp:computeFeatures")
		val cpa = new CPGlobalAlignment(1)
		cpa.verbose = verbose && cpaVerbose
		cpa.redirectLogTo(this)
		cpa.initData(Context(report, passage), featureFunc _)
		cpa.addFertilityFactors(fert1, fert2, splitPredArgFertWeights, includeRowSizeFeatures)
		cpa.addSharingFactors(psaNewNew, psaNewOld, aspNewNew, aspNewOld, psa10, psa01, psa11, asp10, asp01, asp11)
		tempOrdMethod match {
			case FakeTemporalFactors(info) =>
				cpa.addFakeTemporalOrderingFactors(info, tempOrd)
			case TimeSieveTemporalFactors =>
				cpa.addTSTemporalOrderingFactors(tempOrd)
			case NoTemporalFactors => {}
		}
		Profiler.endTask("cp:computeFeatures")
		new CPFeatureRepr(cpa, this)
	}

	// used to calibrate the Constraint weights
	var _avgAlignmentSize = 100d
	def avgAlignmentSize(cpas: Seq[CPGlobalAlignment]): Double = cpas.map(_.alignmentSize).sum / cpas.size
	def setAvgAlignmentSize(cpas: Seq[CPGlobalAlignment]) {
		val a: Double = avgAlignmentSize(cpas)
		_avgAlignmentSize = a
		log("[CPInferenceEngine setAvgAlignmentSize] avg alignment size for %d alignments is %.1f".format(cpas.size, a))
		cpas.foreach(_.setAvgAlignmentSize(a))
	}




	import collection.mutable.Buffer
	/** returns the cplex model for the LP */
	def benchmarkLPrelaxation(da: DocAlignment, addTo: Buffer[LPBenchmark], weightsSize: Double = 0.1d): IloCplex = {

		// set some random dense weights
		val params = theta.copy
		val r = new Random(9001)
		var i = 0
		val n = params.dimension
		while(i < n) {
			theta(i) = r.nextGaussian * weightsSize
			i += 1
		}

		val size = DocMetaAligner.numPossibleAlignments(da.context)
		log("[benchmarkLPrelaxation] decoding alignment with size %d".format(size))
		
		// time an ILP decode
		val (tILP, daExact) = Profiler.getTimeAndValue {
			val fr = computeFeatures(da) 
			fr.features.cpAlignment.decode(theta, relax=false, profile=false)
		}
		log("[benchmarkLPrelaxation] ILP took %.2f seconds".format(tILP))

		// time an LP decode
		val (tLP, (fr, daRel)) = Profiler.getTimeAndValue {
			val fr = computeFeatures(da) 
			val daRel = fr.features.cpAlignment.decode(theta, relax=true, profile=false)
			(fr, daRel)
		}
		log("[benchmarkLPrelaxation] LP took %.2f seconds".format(tLP))
		val cplex = fr.features.cpAlignment.getDecodeCplex(theta, relax=true)

		import edu.jhu.hlt.parma.evaluation.SetBasedEvaluator
		val deltaF1 = SetBasedEvaluator.generousF1(Instance(daExact, daRel))
		val exactF1 = SetBasedEvaluator.generousF1(Instance(da, daExact))
		val relaxF1 = SetBasedEvaluator.generousF1(Instance(da, daRel))
		
		addTo += LPBenchmark(da, size, tILP, tLP, deltaF1, exactF1, relaxF1)
		cplex
	}




	import Snapshot.delta	// syntactic sugar

	def trainStep(examples: Seq[DocAlignmentWithFeatures[CPFeatureRepr]],
			prev: OptStepMeta,
			useConstraintCache: Boolean = true,
			qpOptIter: Int):
			OptStepMeta = {

		// compute most violated constraint (1-slack margin rescaling)
		val (newConstraint, cacheHit) = mostViolatedConstraint(examples, useCache=useConstraintCache)
		val hinge = newConstraint.hinge(theta)	// aka slack
		val addedCons = if(workingSet.size == 0) {
			workingSet.add(newConstraint)
			true
		} else {
			val (prevCons, prevHinge) = workingSet.mostViolated(theta)
			hinge > prevHinge && workingSet.add(newConstraint)
		}

		val tt = VecOps.dot(theta, theta)
		val objective = tt + svmC * hinge
		log("[trainStep] before QP solve: theta.l2^2=%.4f + svmC=%.4f * hinge=%.4f = value=%.4f"
			.format(tt, svmC, hinge, objective))

		// optimize QP over working set
		solve(qpOptIter)

		// package things up for later analysis
		val addedCachedCons = addedCons && cacheHit
		val addedCplexCons = addedCons && !cacheHit
		val x = Some(theta.copy)
		val g = None	//Some(objectiveFunction.getValueGradient)
		new OptStepMeta(prev, hinge, objective, addedCachedCons, addedCplexCons, workingSet.size, qpOptIter, theta=x, gradient=g)
	}

	override def train(examples: Seq[DocAlignmentWithFeatures[CPFeatureRepr]]) {
		val mi = ParmaConfig.getInt("inference.ssvm.cutting-plane-max-iter", 1000)
		train(examples, maxIter=mi, useConstraintCaching=true, qpOptIter=100)
	}
	def train(allExamples: Seq[DocAlignmentWithFeatures[CPFeatureRepr]],
			maxIter: Int = 5000,
			useConstraintCaching: Boolean = true,
			qpOptIter: Int = 100) {

		if(!readParamsInsteadOfTraining.isEmpty) {
			val f = readParamsInsteadOfTraining.get
			log("[CPInferenceEngine train] reading params from %s instead of training".format(f.getPath))
			this.readParameters(f)
			return
		}

		// right now i'm not sure if big alignments are causing a problem for training
		// i do know that parma2 models tend to do a little better than parma on large alignments (when trained on everything)
		val tooBig = ParmaConfig.getInt("inference.ssvm.filter-large-train-alignments", 99999)
		var examples = allExamples.filter(_.features.cpAlignment.size <= tooBig)
		log("[CPInferenceEngine train] given %d examples, %d of them are less than size %d"
			.format(allExamples.size, examples.size, tooBig))

		val lim = ParmaConfig.getInt("inference.num-train-limit", 9999)
		log("[CPInferenceEngine train] lim=" + lim)
		if(examples.size > lim) {
			log("[CPInferenceEngine train] limiting the number of training examples to %d (according to the \"%s\" setting in your config file)"
				.format(lim, "inference.num-train-limit"))
			import Reservoir.Sample
			examples = examples.reservoir(lim)
		}

		val logMemUsage = true

		require(hasPretrained, "you must call preTrainCalibrate before calling train")
		val start = System.currentTimeMillis
		logIf(verbose, "[CPInferenceEngine train] training on %d instances".format(examples.size))

		// do this so that you can call ham.featureName(idx)
		ham.registerFeatures(examples.flatMap(_.features.cpAlignment.alignments.map(_.features)))

		setAvgAlignmentSize(examples.map(_.features.cpAlignment))

		theta.zeroOut
		objectiveFunction.setDirty
		workingSet.clear
		cacheStats.clear

		logIf(verbose, "[CPInferenceEngine] about to train on %d examples)".format(examples.size))

		val epsSlack = 1d / math.sqrt(10+examples.size)
		log("[CPInferenceEngine train] epsSlack=%f".format(epsSlack))

		var cachedInARow = 0
		var nearAvg = 0
		val nearAvgCutoff = 10
		var step = OptStepMeta.initial
		while(!step.done(maxIter=Some(maxIter)) && !(nearAvg > nearAvgCutoff && cachedInARow < nearAvgCutoff)) {
			
			step = trainStep(examples, step, useConstraintCaching, qpOptIter)
			logIf(verbose, "[train] " + step.toString)
			if(logMemUsage)
				logIf(verbose, "[train] memory usage: " + Describe.memoryUsage(timestamp=true))

			if(step.iter % 10 == 0)
				logBiggestParameters(20)

			if(step.addedCachedConstraint)
				cachedInARow += 1
			else
				cachedInARow = 0

			val impr = (step.objectiveExpAvg - step.objective.cur) / step.objectiveExpAvg
			if(math.abs(impr) < epsSlack) {
				nearAvg += 1
				log("[CPInferenceEngine train] small improvement for %d iterations (impr=%f thresh=%f), stopping if this hits %d"
					.format(nearAvg, impr, epsSlack, nearAvgCutoff))
			}
			else {
				nearAvg = 0
				log("[CPInferenceEngine train] significant improvement (impr=%f thresh=%f), resetting nearAvg".format(impr, epsSlack))
			}
		}
		warnIf(step.iter > maxIter, "hit max iteration (%d): %s".format(maxIter, step))
		logIf(verbose, "[CPInferenceEngine] done training on %d instances in %.1f seconds"
			.format(examples.size, (System.currentTimeMillis-start)/1000d))
		flushLog
	}

	// how many times we needed to call cplex for most violated constraint
	var cplexCalls = 0
	val mvTimeouts = new TimeoutCounter("cplex-mv")

	lazy val rejuv = {
		val v = ParmaConfig.getDouble("inference.ssvm.constraint-rejuv", 1.01d)
		log("[CPInferenceEngine] most violated constraint cache rejuvination = %.3f".format(v))
		v
	}
	/**
	 * may return a constraint that is not violated!
	 * returns (most-violated-constraint, whether-it-came-from-cache)
	 */
	def mostViolatedConstraint(
			examples: Seq[DocAlignmentWithFeatures[CPFeatureRepr]],
			useCache: Boolean = true): (Constraint, Boolean) = {

		logIf(verbose, "[mostViolatedConstraint] starting, useCache=" + useCache)
		Profiler.startTask("CP:mostViolatedConstraint")
		val cpas: Seq[CPGlobalAlignment] = examples.map(_.features.cpAlignment)

		if(useCache && workingSet.size > 0) {
			Profiler.startTask("CP:mostViolatedConstraint:cached")
			val cacheConstraint = new Constraint(theta.dimension)
			cpas.foreach(_.addMostViolatedConstraintCached(theta, cacheConstraint))
			cacheConstraint *= (1d/examples.size)
			val newCHinge = cacheConstraint.hinge(theta)
			logIf(verbose, "most violated constraint from cache %s => %.3f".format(cacheConstraint, newCHinge))
			val (oldC, oldH) = workingSet.mostViolated(theta)
			Profiler.endTask("CP:mostViolatedConstraint:cached")
			val impr = (newCHinge - oldH) / newCHinge
			log("[mostViolatedConstraint] from cache, hinge=%f oldHinge=%f impr=%f rejuv=%f"
				.format(newCHinge, oldH, impr, rejuv))
			if(impr > rejuv && impr > 1e-2 && oldH > 0d) {
				cacheStats.hit
				Profiler.endTask("CP:mostViolatedConstraint")
				cacheConstraint.redirectLogTo(this)
				return (cacheConstraint, true)
			}
		}

		Profiler.startTask("CP:mostViolatedConstraint:cplex")
		cacheStats.miss
		cplexCalls += 1
		val timeMult = math.min(5d, math.pow(cplexCalls, 0.2d))

		val threads = ParmaConfig.getInt("inference.ssvm.decode-threads", 1)
		val newConstraint =
			if(threads > 1) parallelMV(cpas, threads, lpRelaxedTraining, timeLimitMultiplier=timeMult)
			else {
				val c = new Constraint(theta.dimension)
				c.redirectLogTo(this)
				cpas.foreach(_.addMostViolatedConstraint(theta, c, relax=lpRelaxedTraining, timeLimitMultiplier=timeMult))
				c
			}
		newConstraint *= (1d/examples.size)
		require(!newConstraint.containsBadValues())

		val debug = false
		if(debug) {
			// it seems that some of the indices in the constraints do not correspond to
			// feature (names that we know about)
			// right now, i'm going to check if this is the case here, and if it is,
			// try to trace it back to where the mis-indexing is being introduced
			
			// this doesn't seem to happen when I use a very small feature set (e.g. Transducer+TED)

			// although, i must say, that even if there are constraint values that are non-zero in
			// zero dimensions of the features, i don't see how this would cause the QP to be unbounded
			// however, this seems like a high-priority enough issue that i'm going to track it down

			val seen = new java.util.HashSet[String]
			val df: DVec = newConstraint.getDeltaFeatures
			for((idx, weight) <- df if weight != 0d) {
				val n = featureName(idx)
				if(n == null) warn("should not be null, idx=" + idx + ", weight=" + weight)
				else if(!seen.add(n)) warn("this name is not uniq: " + n)
				//require(n != null, "idx = " + idx)
				//require(seen.add(n), "n = " + n)
			}

			import java.io.File
			val f = new File("/home/hltcoe/twolfe/fall2013/parma/diagnostics/constraints/%d.cons".format(workingSet.size))
			val writer = FileUtils.getWriter(f)
			for((i,v) <- df)
				writer.write("%d %f\n".format(i, v))
			writer.close()
		}

		val curTimeouts = new TimeoutCounter("cur-iter")
		cpas.foreach(curTimeouts += _.mvTimeouts)
		mvTimeouts += curTimeouts
		logIf(verbose, "[mostViolatedConstraint] curIter=%s all=%s".format(curTimeouts, mvTimeouts))

		// check that this really is the most violated constraint!
		if(verbose) {
			val hstar = newConstraint.hinge(theta)
			logIf(verbose, "cplex produced new most violated %s => %.5f".format(newConstraint, hstar))
			if(workingSet.size > 0) {
				val (oldC, oldH) = workingSet.mostViolated(theta)
				//assert(hstar > oldH - 1e-3,
				warnIf(hstar < oldH - 1e-3,	// this may not be true due to time limits on cplex
					"worst constraint in working set %s => %.5f\nworst constraint cplex found %s => %.5f\nnew constraint in working set? %s"
						.format(oldC, oldH, newConstraint, hstar, workingSet.iterator.contains(newConstraint)))
			}
		}

		Profiler.endTask("CP:mostViolatedConstraint")
		Profiler.endTask("CP:mostViolatedConstraint:cplex")
		(newConstraint, false)
	}


	abstract sealed class CPGDecoderRunnableMode(val lpRelaxed: Boolean, val timeLimitMult: Double)
	case class MostViolated(override val lpRelaxed: Boolean = false, override val timeLimitMult: Double = 1d) extends CPGDecoderRunnableMode(lpRelaxed, timeLimitMult)
	case class Decode(val offset: Double, override val lpRelaxed: Boolean = false, override val timeLimitMult: Double = 1d) extends CPGDecoderRunnableMode(lpRelaxed, timeLimitMult)

	final class CPGDecoderRunnable(val theta: DVec, val mode: CPGDecoderRunnableMode) extends Runnable {
		val cpas = new ArrayBuffer[CPGlobalAlignment]
		val constraint = new Constraint(theta.dimension)
		var alignments = IndexedSeq[DocAlignment]()
		def addCPA(cpa: CPGlobalAlignment) { cpas += cpa }
		override def run {
			mode match {
			case MostViolated(lpRelaxed, timLim) =>
				cpas.foreach(_.addMostViolatedConstraint(theta, constraint, profile=false, relax=lpRelaxed, timeLimitMultiplier=timLim))
				require(!constraint.containsBadValues())
			case Decode(ofs, lpRelaxed, timLim) =>
				alignments = cpas.map(_.decode(theta, offset=ofs, profile=false, relax=lpRelaxed)).toIndexedSeq
			}
			print("*")
		}
	}

	private def shutdownAndAwaitTermination(pool: ExecutorService, timeoutInSecs: Int) {
		pool.shutdown(); // Disable new tasks from being submitted
		try {
			// Wait a while for existing tasks to terminate
			if (!pool.awaitTermination(timeoutInSecs, TimeUnit.SECONDS)) {
				pool.shutdownNow(); // Cancel currently executing tasks
			// Wait a while for tasks to respond to being cancelled
			if (!pool.awaitTermination(timeoutInSecs, TimeUnit.SECONDS))
				warn("Pool did not terminate");
			}
		} catch {
			case ie: InterruptedException =>
				// (Re-)Cancel if current thread also interrupted
				pool.shutdownNow();
				// Preserve interrupt status
				Thread.currentThread().interrupt();
		}
	}

	def parallelMV(cpas: Seq[CPGlobalAlignment], nThreads: Int, lpRelaxed: Boolean, timeLimitMultiplier: Double = 1d): Constraint = {
		logIf(verbose, "[CPInferenceEngine parallel most violated constraint] decoding %d alignments with %d threads"
			.format(cpas.size, nThreads))
		val start = System.currentTimeMillis
		val pool = Executors.newFixedThreadPool(nThreads)
		val decoders = new ArrayBuffer[CPGDecoderRunnable]
		cpas.foreach(cpa => {
			val d = new CPGDecoderRunnable(theta, MostViolated(lpRelaxed=lpRelaxed))
			d.addCPA(cpa)
			decoders += d
			pool.execute(d)
		})
		val wait = (((cpas.size / nThreads) + 2d) * CPGlobalAlignment.cplexTestTimeLimit).toInt
		shutdownAndAwaitTermination(pool, wait)
		log(" done.")
		val c = new Constraint(theta.dimension)
		decoders.foreach(c += _.constraint)
		logIf(verbose, "[CPInferenceEngine parallel most violated constraint] done in %.1f seconds"
			.format((System.currentTimeMillis - start)/1000d))
		c
	}

	def parallelAlign(frs: Seq[CPFeatureRepr], nThreads: Int, offset: Double, lpRelaxed: Boolean): Seq[DocAlignment] = {
		log("[CPInferenceEngine parallel align] decoding %d alignments with %d threads".format(frs.size, nThreads))
		val start = System.currentTimeMillis
		val pool = Executors.newFixedThreadPool(nThreads)
		val decoders = new ArrayBuffer[CPGDecoderRunnable]
		frs.foreach(fr => {
			val d = new CPGDecoderRunnable(theta, Decode(offset, lpRelaxed=lpRelaxed))
			d.addCPA(fr.cpAlignment)
			decoders += d
			pool.execute(d)
		})
		val wait = (((frs.size / nThreads) + 2d) * CPGlobalAlignment.cplexTestTimeLimit).toInt
		shutdownAndAwaitTermination(pool, wait)
		log(" done.")
		val c = new Constraint(theta.dimension)
		val das = decoders.map(_.alignments(0))
		log("[CPInferenceEngine parallel align] done in %.1f seconds"
			.format((System.currentTimeMillis - start)/1000d))
		das
	}


	def writeoutQP(f: File) {
		log("writing out QP to " + f.getPath)
		val cons = workingSet.map(c => QPConstraint(c.getDeltaFeatures, c.getLoss)).toIndexedSeq
		val problem = QP(theta.dimension, svmC, cons)
		problem.toFile(f)
	}


	/**
	 * returns the primal value given the current working set
	 */
	protected def solve(maxIter: Int) {
		Profiler.startTask("CP:solve-primal")

		logIf(verbose, "[solve] starting, maxIter=" + maxIter)
		val (mvcs, mvhs) = workingSet.mostViolated(theta)
		logIf(verbose, "[solve] (start) mostViolated.hinge %s hinge=%.4g".format(mvcs, mvhs))

		// *** Implementation Note ***
		// don't put a learning rate in here, i have added it as a scale in the gradient function

		var diff: DVec = null

		// LBFGS
		if(useLBFGS) {
			Profiler.startTask("CP:mostViolatedConstraint:LBFGS")
			objectiveFunction.setDirty
			val lbfgs = new LimitedMemoryBFGS(objectiveFunction)
			try { lbfgs.optimize(maxIter) }
			catch {
				case e: cc.mallet.optimize.OptimizationException =>
					warn("lbfgs did not converge")
				case ee: cc.mallet.optimize.InvalidOptimizableException =>
					warn(ee.getMessage)
					warn("if you are not training on very small datasets " +
						"or with oracle features, this is probably a real problem")
			}
			diff = theta.copy
			Profiler.endTask("CP:mostViolatedConstraint:LBFGS")
		}

		// Steepest Descent
		Profiler.startTask("CP:mostViolatedConstraint:GradientAscent")
		objectiveFunction.setDirty
		val sd = new GradientAscent(objectiveFunction)
		try { sd.optimize(maxIter) }
		catch {
			case e: cc.mallet.optimize.OptimizationException =>
				warn("lbfgs did not converge")
			case ee: cc.mallet.optimize.InvalidOptimizableException =>
				warn(ee.getMessage)
				warn("if you are not training on very small datasets " +
					"or with oracle features, this is probably a real problem")
		}
		Profiler.endTask("CP:mostViolatedConstraint:GradientAscent")

		// compare LBFGS solution to Steepest Descent
		if(diff != null) {
			VecOps.add(diff, theta, -1d)
			log("[solve] diff l2=%.3g l1=%.3g".format(diff.l2, diff.l1))
			warnIf(diff.l2 > 0.01,
				"LBFGS may not have converged, it differs from the steepest descent solution significantly")
		}

		val (mvce, mvhe) = workingSet.mostViolated(theta)
		logIf(verbose, "[solve] (end) mostViolated.hinge %s hinge=%.8g".format(mvce, mvhe))

		Profiler.endTask("CP:solve-primal")
	}

	val objectiveFunction = new Optimizable.ByGradientValue {
		var verbose = false
		var valueDirty = true
		var gradDirty = true
		private[this] var valueCache = 0d
		private[this] var gradCache = theta.copy
		gradCache.zeroOut
		def setDirty {
			valueDirty = true
			gradDirty = true
		}
		override def getValue: Double = {
			if(valueDirty) {
				Profiler.startTask("CP:mostViolatedConstraint:getValue")

				val (cons, hinge) = workingSet.mostViolated(theta)
				val tt = VecOps.dot(theta, theta)
				valueCache = -1d * (tt + svmC * hinge)

				assert(hinge >= 0d)
				valueDirty = false
				if(verbose) {
					logParams("[train getValue] theta=", theta)
					log("[train getValue] theta.l2^2=%.4f + svmC=%.4f * slack=%.4f = value=%.4f"
						.format(tt, svmC, hinge, valueCache))
				}
				Profiler.endTask("CP:mostViolatedConstraint:getValue")
			}
			valueCache
		}
		override def getValueGradient(buf: Array[Double]) {
			if(gradDirty) {
				Profiler.startTask("CP:mostViolatedConstraint:getValueGradient")

				gradCache.zeroOut
				val (cons, hinge) = workingSet.mostViolated(theta)
				cons.accumGradient(theta, gradCache)
				gradCache *= svmC
				VecOps.add(gradCache, theta, -2d)
				gradCache *= learningRate

				if(verbose) {
					log("[train getValueGradient] learningRate=%.4g".format(learningRate))
					logParams("[train getValueGradient] grad=", gradCache)
					logParams("[train getValueGradient] theta=", theta)
					log("[train getValueGradient] slack=%.4f".format(hinge))
				}

				gradDirty = false
				Profiler.endTask("CP:mostViolatedConstraint:getValueGradient")
			}
			System.arraycopy(gradCache.getArray, 0, buf, 0, buf.length)
		}
		def getValueGradient: DVec = {
			val grad = Array.ofDim[Double](theta.dimension)
			getValueGradient(grad)
			new DVec(grad)
		}
		override def getNumParameters = theta.dimension
		override def getParameter(i: Int) = throw new RuntimeException("please call bulk get!")
		override def getParameters(buf: Array[Double]) {
			System.arraycopy(theta.getArray, 0, buf, 0, theta.dimension)
		}
		override def setParameter(i: Int, v: Double) = throw new RuntimeException("please call bulk set!")
		override def setParameters(buf: Array[Double]) {
			setDirty
			val old = theta.copy
			System.arraycopy(buf, 0, theta.getArray, 0, theta.dimension)
			if(verbose) {
				//logParams("[train setParameters] old", old)
				logParams("[train setParameters] new", theta)
				log("")
			}
			//import util.Random
			//verbose = (Random.nextInt(100) == 0)
		}
	}

	override def align(frs: Seq[CPFeatureRepr]): Seq[DocAlignment] = {
		log("[CPInferenceEngine align] setting avgAlignmentSize=%f before aligning".format(_avgAlignmentSize))
		frs.foreach(_.cpAlignment.setAvgAlignmentSize(_avgAlignmentSize))
		val threads = ParmaConfig.getInt("inference.ssvm.decode-threads", 1)
		if(threads == 1) {
			log("[CPInferenceEngine align] single threaded align")
			frs.map(_.cpAlignment.decode(theta, decodeScoreOffset))
		}
		else {
			log("[CPInferenceEngine align] using parallel align with %d threads".format(threads))
			parallelAlign(frs, threads, decodeScoreOffset, lpRelaxedDecoding)
		}
	}

	/**
	 * this adds a constant to the similarity of every CPAlignment and takes
	 * the best offset value on the given dev data
	 */
	override def postTrainCalibrate(
			examples: Seq[DocAlignmentWithFeatures[CPFeatureRepr]],
			loss: EvaluationLoss) {

		// note, i can't do svmC tuning here, because that would require training a whole other model

		val nThreads = ParmaConfig.getInt("inference.ssvm.decode-threads", 1)
		val offsets = (-1d to 1d by 0.25d).toIndexedSeq
		val losses = offsets.map(offset => {
			val hyp: Seq[DocAlignment] = parallelAlign(examples.map(_.features), nThreads, offset, lpRelaxedDecoding)
			val gold: Seq[DocAlignment] = examples.map(_.alignment)
			val instances = hyp.zip(gold).map(hg => new Instance(hg._1, hg._2))
			loss(instances)
		})
		val minLoss = losses.min
		val weights = losses.map(l => math.exp(20d * (minLoss - l)))
		var n = 0d
		var d = 0d
		for((ofs, w) <- offsets.zip(weights)) {
			n += ofs * w
			d += w
		}
		this.decodeScoreOffset = n / d

		log("[CPInferenceEngine postTrainCalibrate] " + offsets.zip(losses).mkString("\n"))
		log("[CPInferenceEngine postTrainCalibrate] set offset to " + this.decodeScoreOffset)
	}

	override def preTrainCalibrate(examples: Seq[DocAlignment]) {
		log("[MM] preTrainCalibrate")
		ham.preTrainCalibrate(examples)
		hasPretrained = true
	}

	override def featureName(index: Int): String = {
		if(readParamsInsteadOfTraining.isEmpty) {
			for(p <- quadraticParams)
				if(p.index == index)
					return p.name
			// otherwise, must be similarity feature
			ham.featureName(index)
		}
		else "implement-feature-name-saving-plz"
	}

	override def parameters: edu.jhu.hlt.parma.types.DVec = theta.copy
}

/**
 * use cplex to solve the primal CP (as well as the separation oracle as before)
 * @deprecated please don't use this, this was not thought through and CPLEX cannot solve indefinite QPs
 * TODO need to switch to the dual
 */
class CPInferenceEngineFast extends CPInferenceEngine {

	// don't use this without benchmarking again
	// my first benchmarks show that this actually slightly slows things down
	val warmStart = ParmaConfig.getBoolean("inference.ssvm.warmStart", false)

	/**
	 * tries to do what solve does faster, using cplex for solving my QP
	 *
	 * QP:
	 * min 1/2 x Q x' + c' x
	 * s.t. A x >= b
	 *
	 * Primal SVM-C:
	 * min ||w|| + C xi
	 * s.t. xi >= max(0, loss(x, x~) + w (f(x) - f(~x))) \forall ~x
	 *
	 * Mapping:
	 * x = [ w1 w2 ... wN xi ]
	 * Q = [ 1          ]
	 *     [   1        ]
	 *     [     ...    ]
	 *     [        1   ]
	 *     [          0 ]
	 * c = [ 0 ... 0  1 ]
	 *
	 * each (row in A, entry in b) is an entry in the working set
	 * A = [ f(z1) - f(~z1) 1 ]
	 *     [ f(z2) - f(~z2) 1 ]
	 *     [      ...       1 ]
	 *     [ f(zM) - f(~zM) 1 ]
	 *     [                1 ]
	 * b = [ loss(z1, ~z1) loss(z2, ~z2) ... loss(zM, ~zM) 0 ]
	 */
	override def solve(maxIter: Int) {

		val ones = Array.ofDim[Double](theta.dimension)
		Arrays.fill(ones, 1d)

		Profiler.startTask("CP:solve-primal")

		val cplex = new IloCplex()
		cplex.setParam(IloCplex.IntParam.Threads, CPGlobalAlignment.cplexThreads)
		cplex.setOut(getLogger.getOutputStream)
		val w = cplex.numVarArray(theta.dimension, -9999d, 9999d)
		val xi = cplex.numVar(0d, 999999d)


		// objective
		val qc = cplex.lqNumExpr()
		qc.addTerm(svmC, xi)
		qc.addTerms(ones, w, w)
		qc.addTerm(svmC/100d, xi, xi)	// make the Q matrix full rank (convex QP)
		cplex.addMinimize(qc)


		// constraints
		log("[solveAlt] adding %d constraints".format(workingSet.size))
		require(workingSet.size > 0, "no constraints, problem is trivial")
		for(cons <- workingSet) {
			require(!cons.containsBadValues(checkForNaN=true, checkForInf=true))
			// note that df and loss *do not* need to be scaled by 1/N,
			// as this is done inside Constraint
			val df = cons.getDeltaFeatures
			val loss = cons.getLoss
			val dScore = cplex.scalProd(df.getArray, w)
			cplex.addGe(
				cplex.sum(dScore, xi),	// score(z) - score(\hat{z}) + xi
				loss)					// >= loss(z, \hat{z})
			//log("[solveAlt] deltaFeatures.l2=%.3g df.lInf=%.3g loss=%.3g".format(df.l2, df.lInf, loss))
		}


		// initialize (warm start)
		if(warmStart) {
			val (_, hinge) = workingSet.mostViolated(theta)
			log("[solveAlt] warm start initialization, xi=" + hinge)
			//cplex.addMIPStart(w ++ Array(xi), theta.getArray ++ Array(hinge))	// this is only for MIPs, not QPs
			cplex.setVectors(theta.getArray, null, w, null, null, null)
		}


		// solve
		log("[solveAlt] solving SVM primal with %d constraints using cplex...".format(workingSet.size))
		log("[solveAlt] svmC=%.5g".format(svmC))
		//log("[solveAlt] cplex = " + cplex)	// very long!
		//log("[solveAlt] using algorithm: " + cplex.getAlgorithm)	// prints "0"
		val provablyOptimal = cplex.solve()
		log("[cplex QP solve] proovablyOptimal=" + provablyOptimal)
		if(!cplex.isPrimalFeasible) {
			warn("[solveAlt] not feasible, trying cplex.solve again <bangs head on wall>")
			cplex.solve()
		}
		if(!cplex.isPrimalFeasible) {

			writeoutQP(new File("/home/hltcoe/twolfe/fall2013/parma/diagnostics/bad.qp"))

			warn("...wut, not feasible...")
			for((wi, idx) <- w.zipWithIndex) {
				val wiv = cplex.getValue(wi)
				if(wiv != 0d)
					warn("infeasible-w(%d) = %f".format(idx, wiv))
			}
			warn("xi = " + cplex.getValue(xi))

			var writer = FileUtils.getWriter("/home/hltcoe/twolfe/fall2013/parma/diagnostics/bad-constraints.txt")
			for((cons, idx) <- workingSet.zipWithIndex) {
				warn("infeasible-cons(%d) %s".format(idx, Describe.constraint(cons)))
				writer.write("%f %s\n".format(cons.getLoss, cons.getDeltaFeatures.getArray.mkString(" ")))
			}
			writer.close()
			writer = FileUtils.getWriter("/home/hltcoe/twolfe/fall2013/parma/diagnostics/bad-weights.txt")
			writer.write("xi %f\n".format(cplex.getValue(xi)))
			for((wi, idx) <- w.zipWithIndex)
				writer.write("%d %f\n".format(idx, cplex.getValue(wi)))
			writer.close()
			writer = FileUtils.getWriter("/home/hltcoe/twolfe/fall2013/parma/diagnostics/bad-weights-names.txt")
			for((wi, idx) <- w.zipWithIndex)
				writer.write("%s %f\n".format(featureName(idx), cplex.getValue(wi)))
			writer.close()
			assert(false)
		}
		warnIf(cplex.getStatus.toString != "Optimal",
			"cplex didn't find optimal QP solution: " + cplex.getStatus.toString)


		// read out solution
		val prevTheta = theta.copy
		theta.setBacking(cplex.getValues(w))
		val diff = theta.copy
		VecOps.add(diff, prevTheta, -1d)
		log("[solveAlt] theta.l2=%.4g theta.lInf=%.4g diff.l2=%4g diff.lInf=%.4f xi=%.4g"
			.format(theta.l2, theta.lInf, diff.l2, diff.lInf, cplex.getValue(xi)))
		//logParams("[cplex-primal-solve] old", prevTheta)
		logParams("[cplex-primal-solve] new", theta)
		cplex.end()
		Profiler.endTask("CP:solve-primal")
	}
}

