// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.inference

import edu.jhu.hlt.parma.features.FeatureLoader
import edu.jhu.hlt.parma.feature_interfaces.AlignmentSimilarity
import edu.jhu.hlt.parma.features.generic.SimilarityImplementation
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.evaluation._
import edu.jhu.hlt.parma.diagnostics.GeneralDiagnostics
import edu.jhu.hlt.concrete.Concrete.Communication
import cc.mallet.optimize._
import collection.JavaConversions._
import collection.mutable.{ArrayBuffer, HashSet, HashMap}
import util.Random
import java.io._

import edu.jhu.hlt.parma.annotation.AlignmentScorer

class HAMFeatureRepresentation(
		val labels: Option[java.util.BitSet],		// in the set means positive alignment, TODO possible set
		val alignments: Array[Alignment],
		val features: Array[SVec],
		val report: Document,
		val passage: Document,
		val domain: Option[String],
		val ham: HierarchicalAlignmentModule)
		extends FeatureRepresentation {

	assert(alignments.length == features.length)

	def apply(i: Int) = (alignments(i), features(i))

	def size = alignments.length

	override def inspectFeatures: Option[collection.Map[Alignment, SVec]] = {
		//Some(alignments.zip(features).toMap)
		val f = new HashMap[Alignment, SVec]
		for(i <- 0 until size) {
			// duplicate b/c I don't want to disturb the orig svec
			val nsv = SVec.duplicate(features(i))
			nsv.compact	// compact so that effects are summed in once place
			f += (alignments(i) -> nsv)
		}
		Some(f)
	}

	// this is updated by HAM
	// hackish, but can stay in step with the pipeline
	var scores: Option[Array[Double]] = None

	override def inspectScores: Option[Map[Alignment, Double]] =
		scores match {
			case Some(sc) =>
				Some(alignments.zip(sc).toMap)
			case None =>
				None
		}

	override def controller = ham
}

class HierarchicalAlignmentModule
		extends AlignmentFeatureComputer
		with InferenceEngine[HAMFeatureRepresentation]
		with AlignmentScorer
		with Logging2
		with Serializable {

	var verbose = false
	val debug = true

	val L1_PENALTY = "inference.ham.L1penalty"
	val DEFAULT_THRESHOLD = "inference.ham.threshold.default"
	val SHOULD_NORMALIZE_FEATURES = "inference.ham.normalize.features"

	// String labels for classification
	val ALIGNED = "aligned"
	val NOT_ALIGNED = "!aligned"

	// stores the feature functions, not the feature/weight vector
	val alph = new Alphabet[String]
	val features = FeatureLoader.getFeatures(alph)
	//val featureIndexer = new TwoPartFeatureIndexer[AlignmentSimilarity](features.size, 1<<22)
	val shouldNormalize = ParmaConfig.getBoolean(SHOULD_NORMALIZE_FEATURES, false)
	//val normalizer = new FeatureFunctionNormalizer

	// TODO setup setter like l1
	val useHinge = ParmaConfig.getBoolean("inference.ham.useHinge", true)
	var l2Penalty = ParmaConfig.getDouble("inference.ham.L2penalty", 1d)
	val learningRate = ParmaConfig.getDouble("inference.ham.learningRate", 1e-2)
	private var weights = DVec.rep(0d, 200000)	// TODO measure the size needed!
	var knownFeatures: java.util.BitSet = null		// TODO this will help avoid calling featureName on non-existent features
	var hasPreTrained = false
	var hasTrained = false

	def getThreshold: Double = threshold
	private var threshold = ParmaConfig.getDouble("inference.ham.threshold", 0.2)
	
	var _l1Penalty: Option[Double] = None
	def l1Penalty = _l1Penalty match {
		case Some(p) => p
		case None => ParmaConfig.getDouble(L1_PENALTY, 1.0)
	}
	def setL1Penalty(p: Double) {
		assert(p >= 0d)
		_l1Penalty = Some(p)
	}

	override def featureName(index: Int): String = {
		alph.lookupObject(index)
	}
	/*
	override def featureName(index: Int): String = {
		//if(!hasTrained)
		if(knownFeatures == null)
			throw new RuntimeException("i can't tell you feature names until you've registered the features")
		val debug = false
		if(debug) {
			log("[HAM featureName] looking up feature name for " + index)
			val (outer, inner) = featureIndexer.lookupIndexRaw(index)
			log("[HAM featureName] outer index = " + outer)
			log("[HAM featureName] inner index = " + inner)
		}
		if(knownFeatures.get(index)) {
			val (ff, ffIdx) = featureIndexer.lookupIndex(index)
			if(debug) log("[HAM featureName] feature = " + ff.name)
			val ffLocalName = ff.featureName(ffIdx)
			assert(ffLocalName != null)
			if(ffLocalName == null)	null	// feature did not use this index
			else ff.name + "@" + ffLocalName
		}
		else null
	}
	*/

	/** returns a copy of the paremeters */
	override def parameters: DVec = weights.copy

	def parametersNoCopy: DVec = weights

	override def readParameters(f: java.io.File) {
		log("[HierarchicalAlignmentModule readParameters] from " + f.getPath)
		val r = FileUtils.getReader(f)
		val lineOne = r.readLine
		require(lineOne == "HierarchicalAlignmentModule", "unknown class: " + lineOne)
		r.readLine	// train time
		r.readLine	// l2 penalty
		r.readLine	// use hinge
		r.readLine	// warning
		val stage = weights.copy
		val n = weights.dimension
		var i = 0
		while(i < n) {
			val line = r.readLine
			//log("line(%d) = \"%s\"".format(i, line))
			stage(i) = line.toDouble
			i += 1
		}
		warnIf(r.ready, "did you match the dimension correctly?")
		r.close
		weights = stage
	}

	override def writeoutParameters(f: java.io.File) {
		log("[HierarchicalAlignmentModule writeoutParameters] to " + f.getPath)
		val w = FileUtils.getWriter(f)
		w.write("HierarchicalAlignmentModule\n")
		w.write("trained at " + new java.util.Date().toString + "\n")
		w.write("l2Penalty=%f\n".format(l2Penalty))
		w.write("useHinge=%s\n".format(useHinge))
		w.write("other parameters are not checked, make sure you got them right!\n")
		w.write(weights.getArray.mkString("\n") + "\n")
		w.close
	}

	def writeoutParametersHumanFriendly(f: File) {
		val ps = new PrintStream(new FileOutputStream(f), false, FileUtils.DEFAULT_ENCODING)
		//classifier.get.print(ps)
		for(i <- 0 until weights.dimension) {
			val name = featureName(i)
			if(name == null)
				assert(weights(i) == 0d)
			else
				ps.println("%.100s %.3f".format(name, weights(i)))
		}
		ps.println("learning rate = %.2g".format(learningRate))
		//ps.println("L1 penalty = %.2g".format(l1Penalty))
		ps.println("L2 penalty = %.2g".format(l2Penalty))
		ps.println("threshold = %.2g".format(threshold))
		ps.println("shouldNormalize = " + shouldNormalize)
		ps.println(new java.util.Date().toString)
		ps.close
	}

	def predict(sv: SVec): Double = {
		val wx = VecOps.dot(weights, sv)
		val extreme = 20d
		if(wx < -extreme || wx > extreme)
			warn("very extreme probability: " + wx)
		if(useHinge) wx
		else 1d / (1d + java.lang.Math.exp( -wx ))
	}
	
	// for trait AlignmentFeatureComputer
	var cfSpinner = 0
	override def computeFeatures(alignment: Alignment, report: Document, passage: Document, domain: Option[String]): SVec = {
	  
	  	val debug = cfSpinner % 1500 == 0
		cfSpinner += 1
		lazy val savedSvBufs = Array.ofDim[SVec](features.size)

		if(debug) {
			log("\n[HAM computeFeatures] about to call %d features for %s"
				.format(features.size, Describe.alignment(alignment, report, passage)))
		}

		val bufSize = 500
		val sv = new SVec(bufSize + 50)
		var svBuf = new SVec(bufSize)
		try {
			for((ff, ffi) <- features.zipWithIndex) {
				if(debug) log("[HAM computeFeatures] about to call featurize on " + ff.name)
				Profiler.time("features:" + ff.name, Unit =>
					ff.featurize(svBuf, alignment, report, passage)
				)
				if(debug) {
					//log("[HAM computeFeatures] " + Describe.svec(svBuf))
					assert(!svBuf.containsBadValues(checkForNaN=true, checkForInf=true))

					lazy val msg = "these features are too big! %s gave %s".format(ff.name, Describe.svec(svBuf))
					//require(svBuf.lInf < 5d, msg)
					warnIf(svBuf.lInf > 5d, msg)
				}

				// OLD:
				//featureIndexer.reindex(ff, svBuf, sv)	// sv += svBuf
				// NEW: (reindexer is not used because features now produce globally valid indices)
				sv ++= svBuf

				if(!debug) svBuf.clear
				else {
					savedSvBufs(ffi) = svBuf
					svBuf = new SVec(bufSize)
				}
				if(debug) log("\n")
			}
		} catch {
			case nse: NoSuchElementException =>
				warn(nse.getStackTrace.mkString("\n"))
				throw new RuntimeException("are you sure you ran preTrainCalibrate?", nse)
		}

		// TODO get domain adaptation back!
		val unNormalizedSV = sv
		//val unNormalizedSV = DomainAdaptation.frustratinglySimple(domain, sv)
		if(verbose && domain.isEmpty) log("[HAM] skipping domain adaptation")

		// all you need to know is one domain: what you're producing feautres for
		// you will have a few extra features, but they are more interprettable
		
		val ret = if(shouldNormalize)
			throw new RuntimeException("re-implement me")
		else unNormalizedSV

		// TODO take these out unless debugging is enabled, and move to FeatureSet!!!
		if(ret.containsBadValues(checkForNaN=true, checkForInf=true)) {
			warn(Describe.svec(ret))
			throw new RuntimeException("build a feature vector with bad values!")
		}
		lazy val s = Describe.svec(ret)
		ret.rawIndices.foreach(i => {
			require(i >= 0, s + "\n" + i)
			require(i < weights.dimension, s + "\n" + i)
		})

		if(debug) {
			log("final features = " + Describe.svec(ret))
			for((ff, svBuf) <- features.zip(savedSvBufs))
				log("%s produced %s".format(ff.name, Describe.svec(svBuf)))
			log("\n")
		}

		ret
	}
	
	// for trait InferenceEngine
	override def computeFeatures(da: DocAlignment): DocAlignmentWithFeatures[HAMFeatureRepresentation] = {
		val fr = computeFeatures(da.report, da.passage, da.domain)
		var i = 0
		val n = fr.size
		val labels = new java.util.BitSet(n)
		while(i < n) {
			val a = fr.alignments(i)
			labels.set(i, da.possibleAlignments.contains(a))
			i = i + 1
		}
		val newFR = new HAMFeatureRepresentation(Some(labels), fr.alignments, fr.features, da.report, da.passage, da.domain, this)
		newFR.scores = fr.scores
		new DocAlignmentWithFeatures(da, newFR)
	}

	// for trait InferenceEngine
	override def computeFeatures(report: Document, passage: Document, domain: Option[String]): HAMFeatureRepresentation = {
		val apa = DocMetaAligner.allPossibleAlignments(report, passage)
		val n = apa.size
		log("[HAM] computeFeatures, apa.size=%d, domain=%s".format(apa.size, domain))
		assert(n > 0)
		val alignments = Array.ofDim[Alignment](n)
		val features = Array.ofDim[SVec](n)
		val scores = Array.ofDim[Double](n)
		var idx = 0
		while(idx < n) {
			val a = apa(idx)
			alignments(idx) = a
			features(idx) = computeFeatures(a, report, passage, domain)
			if(hasTrained)
				scores(idx) = predict(features(idx))
			idx += 1
		}
		val fr = new HAMFeatureRepresentation(None, alignments, features, report, passage, domain, this)
		if(hasTrained)
			fr.scores = Some(scores)
		fr
	}

	override def preTrainCalibrate(examples: Seq[DocAlignment]) {
		
		// initialize feature functions
		log("[HAM] setting up %d features on %d examples"
			.format(features.size, examples.size))
		features.foreach(asf => {

			// register this feature function with the feature indexer
			//featureIndexer.lookupOuterIndex(asf, addIfNotPresent=true)

			// allow this feature to set itself up
			if(verbose) log("[HAM] calling setup on "+asf.name)
			Profiler.time("setup:" + asf.name, Unit => asf.setup(examples))
		})
		
		// calculate mean and variance for feature normalizer
		if(shouldNormalize) {
			throw new RuntimeException("reimplement me!")
		}
		hasPreTrained = true
	}

	def registerFeatures(features: Seq[SVec]) {
		if(knownFeatures == null)
			knownFeatures = new java.util.BitSet
		//for(e <- examples; sv <- e.features.features)
		//	sv.rawIndices.foreach(knownFeatures.set(_))
		features.foreach(_.rawIndices.foreach(knownFeatures.set(_)))
	}

	override def train(allExamples: Seq[DocAlignmentWithFeatures[HAMFeatureRepresentation]]) {

		assert(hasPreTrained, "you need to call preTrainCalibrate before you can do this")

		val lim = ParmaConfig.getInt("inference.num-train-limit", 99999)
		val examples: Seq[DocAlignmentWithFeatures[HAMFeatureRepresentation]] =
			if(allExamples.size > lim) {
				log("[HAM train] limiting the number of training examples to %d (according to the \"%s\" setting in your config file)"
					.format(lim, "inference.num-train-limit"))
				import Reservoir.Sample
				implicit val r = new Random(9001)
				allExamples.reservoir(lim)
			}
			else allExamples

		registerFeatures(examples.flatMap(_.features.features))

		val func = new Optimizable.ByGradientValue {
			private[this] var valueDirty = true
			private[this] var valueCache = 0d
			private[this] var gradientDirty = true
			private[this] var gradientCache = Array.ofDim[Double](weights.dimension)
			def partialValue(dawf: DocAlignmentWithFeatures[HAMFeatureRepresentation]): Double = {
				val fr = dawf.features
				val labels = fr.labels.get
				var loss = 0d
				var aIdx = 0
				val nAlignments = fr.size
				while(aIdx < nAlignments) {

					val wx = VecOps.dot(weights, fr.features(aIdx))
					val coef = if(labels.get(aIdx)) 1d else -1d

					if(useHinge)
						loss = loss + math.max(1d - coef * wx, 0d)
					else
						loss = loss + java.lang.Math.log1p( java.lang.Math.exp( coef * -wx ) )

					aIdx = aIdx + 1
				}
				- loss
			}
			def regularizerValue: Double = {
				val l2 = weights.l2
				- l2 * l2 * l2Penalty / 2d
			}
			def partialGradient(dawf: DocAlignmentWithFeatures[HAMFeatureRepresentation], gradBuf: Array[Double]) {
				val fr = dawf.features
				val labels = fr.labels.get
				var aIdx = 0
				val nAlignments = fr.size
				while(aIdx < nAlignments) {
					val x = fr.features(aIdx)
					val wx = VecOps.dot(weights, x)

					if(useHinge) {
						val coef = if(labels.get(aIdx)) 1d else -1d
						if(wx * coef < 1d)
							VecOps.addWithScale(x, gradBuf, coef)
					} else {
						val p = 1d / (1d + java.lang.Math.exp( -wx ))
						assert(!java.lang.Double.isInfinite(p) && p > 0d && p < 1d, "wx=%.3g p=%.6f".format(wx, p))
						val coef = (if(labels.get(aIdx)) 1d else 0d) - p	// actual - expected
						VecOps.addWithScale(x, gradBuf, coef)
					}

					aIdx = aIdx + 1
				}
			}
			def regularizerGradient(gradBuf: Array[Double]) {
				var i = 0
				val n = gradBuf.length
				assert(n == weights.dimension)
				while(i < n) {
					gradBuf(i) = gradBuf(i) - weights(i) * l2Penalty
					i = i + 1
				}
			}
			override def getValue: Double = {
				if(valueDirty) {
					val value = examples.map(partialValue).sum
					val reg = regularizerValue
					log("[HAM train] getValue() called, value = " + value + ", reg = " + reg + ", total = " + (value + reg))
					valueCache = value + reg
					valueDirty = false
				}
				valueCache
			}
			override def getValueGradient(buf: Array[Double]) {
				if(gradientDirty) {
					assert(gradientCache.length == buf.length)
					java.util.Arrays.fill(gradientCache, 0d)
					examples.foreach(dawf => partialGradient(dawf, gradientCache))
					val grad = new DVec(gradientCache)
					grad *= (1d / examples.size)
					regularizerGradient(gradientCache)
					if(learningRate != 1d)
						grad *= learningRate
					assert(learningRate != 0d)
					log("[HAM train] getValueGradient() called, learningRate=%.2g grad.l1=%.1f grad.l2=%.1f"
						.format(learningRate, grad.l1, grad.l2))
					gradientDirty = false
				}
				java.lang.System.arraycopy(gradientCache, 0, buf, 0, gradientCache.length)
			}
			override def getNumParameters: Int = weights.dimension
			override def getParameter(idx: Int): Double = weights(idx)
			override def getParameters(buf: Array[Double]) {
				weights.copyTo(buf)
			}
			override def setParameter(idx: Int, value: Double) {
				valueDirty = true
				gradientDirty = true
				weights(idx) = value
			}
			override def setParameters(buf: Array[Double]) {
				valueDirty = true
				gradientDirty = true
				weights.setBacking(buf)
			}
		}
		
		Profiler.time("train-lbfgs", Unit => {
			val lbfgs = new LimitedMemoryBFGS(func)
			lbfgs.setTolerance(1e-9)
			try {
				lbfgs.optimize
				assert(lbfgs.isConverged)
			} catch {
				case oe: OptimizationException =>
					log(oe.getMessage)
					//assert(false)
					log("OHNOz!! LBFGS CAN NOT CONVERGE!!")
			}
		})
		hasTrained = true
	}

	
	/**
	 * make predictions on all of the dev alignments
	 * tune threshold to get best F1
	 */
	def postTrainCalibrate(examples: Seq[DocAlignmentWithFeatures[HAMFeatureRepresentation]],
			loss: EvaluationLoss, thresholdsToTry: Seq[Double]) {
		assert(hasPreTrained, "you need to call preTrainCalibrate before you can do this")
		log("calculating losses for %d parameter settings...".format(thresholdsToTry.size))
		optimizeThreshold(examples, loss, thresholdsToTry)	
	}

	private def optimizeThreshold(examples: Seq[DocAlignmentWithFeatures[HAMFeatureRepresentation]],
			loss: EvaluationLoss, thresholdsToTry: Seq[Double]) {
		val gold = examples.map(_.alignment)
		val lossesForSure = thresholdsToTry.map(t => {
			val predictions = align(examples.map(_.features), t)
			val instances = (predictions zip gold).map(gh => new Instance(gh._1, gh._2))
			(t, loss(instances))
		})

		log("[optmizeThreshold] thresholds with lowest loss according to " + loss.name)
		for ((t, l) <- lossesForSure.sortBy(_._2).take(5))
			log("threshold = %.2f \t loss = %.3f".format(t, l))
			
		val bestThresholdForSure = lossesForSure.minBy(_._2)._1
		log("after tuning on dev set: threshold %.2f => %.2f".format(threshold, bestThresholdForSure))
		threshold = bestThresholdForSure
	}
	
	override def postTrainCalibrate(examples: Seq[DocAlignmentWithFeatures[HAMFeatureRepresentation]], loss: EvaluationLoss) {
		val thresholds =
			if(useHinge) (-3d to 2d by 0.07d)
			else (0.01 to 0.99 by 0.04)
		postTrainCalibrate(examples, loss, thresholds)
	}
	
	
	override def align(frs: Seq[HAMFeatureRepresentation]): Seq[DocAlignment] = align(frs, threshold)
	
	def align(frs: Seq[HAMFeatureRepresentation], thresh: Double): Seq[DocAlignment] = {
		assert(hasPreTrained, "you need to call preTrainCalibrate before you can do this")

		val debug = false
		
		frs.map(fr => {
			val scores = Array.ofDim[Double](fr.size)
			val alignments = new ArrayBuffer[Alignment]
			var idx = 0
			val n = fr.size
			while(idx < n) {
				scores(idx) = predict(fr.features(idx))
				if(debug) log("[HAM align] score=%.2f thresh=%.2f add?=".format(scores(idx), thresh, scores(idx) >= thresh))
				if(scores(idx) >= thresh)
					alignments += fr.alignments(idx)
				idx = idx + 1
			}

			// update this feature representation's scores
			fr.scores = Some(scores)

			val id = "r%s_p%s".format(fr.report.id, fr.passage.id)
			val domain = Some("HAM")
			val da = new DocAlignment(id, domain, fr.report, fr.passage, alignments.toSet, Set())
			assert(da.exactlyPossibleAlignments.size == 0)
			GeneralDiagnostics.checkDocAlignment(da)
			da
		})
	}	

	// for trait AlignmentScorer in TurkerScorer
	override def score(a: Alignment, report: Document, passage: Document, domain: Option[String]): Double =
		predict(computeFeatures(a, report, passage, domain))

	/**
	 * returns avg log-likelihood per alignment
	 */
	override def score(da: DocAlignment, domain: Option[String]): Double = {
		assert(hasPreTrained, "you need to call preTrainCalibrate before you can do this")
		if(useHinge)
			warn("trained for hinge but getting a log-likelihood!")
		var logL = 0d
		var n = 0
		for(a <- DocMetaAligner.allPossibleAlignments(da.report, da.passage)) {
			val sv = computeFeatures(a, da.report, da.passage, domain)
			val p = predict(sv)
			if(da.sureAlignments.contains(a))
				logL += math.log(p)
			else
				logL += math.log(1d - p)
			n += 1
		}
		assert(n > 0, "can't score an empty alignment")
		logL / n
	}

}

/**
 * produce an alignment file with scores
 * see on google docs: Producing Parma Alignments/Scores
 */
object HierarchicalAlignmentModule extends Logging2 {

	def main(args: Array[String]) {
		if(args.length < 5 || args.length > 6) {
			println("please provide:")
			println("1) a Concrete Communications file")
			println("2) a mentions file")
			println("3) TSV: (hit-id/alignment-id) (report-doc-id) (passage-doc-id)")
			println("4) a serialized HAM model")
			println("5) an output alignment file")
			println("6) [optional] domain string that these docs come from")
			return
		}
		val commFile = new File(args(0))
		val mentionFile = new File(args(1))
		val docIdPairFile = new File(args(2))
		val modelFile = new File(args(3))
		val outFile = new File(args(4))
		val domain = if(args.length == 6) Some(args(5)) else None

		log("reading HAM model from %s...".format(modelFile.getPath))
		val ois = new ObjectInputStream(new FileInputStream(modelFile))
		val ham = ois.readObject.asInstanceOf[HierarchicalAlignmentModule]
		ois.close

		val mentions: Seq[MentionFileUtil.MentionRef] = MentionFileUtil.readMentionsFrom(mentionFile)

		log("reading documents pairs to align from %s...".format(docIdPairFile.getPath))
		val docPairs = DocIdPairUtil.readDocIdPairs(docIdPairFile)

		// only read in the communications that are needed (appear in a doc pair)
		val needed: Set[String] = docPairs.flatMap(dp => List(dp.reportId, dp.passageId)).toSet
		val keep = (c: Communication) => needed.contains(c.getGuid.getCommunicationId)
		val docs: Map[String, DocumentBuilder] =
			ConcreteWrapper.getCommunicationsFrom(commFile, keep)
			.map(c => (c.getGuid.getCommunicationId, new RichConcreteDocBuilder(c)))
			.toMap

		def addTo(db: DocumentBuilder): Bijection[Mention, MentionFileUtil.MentionRef] = {
			// maintain a mapping between the mention ids passed in in the mentions file
			// and the pred/args in the documents themselves
			val mapping = new MutableBijection[Mention, MentionFileUtil.MentionRef]
			mentions.filter(_.docId == db.id).foreach(mr => {
				mr.toPredArg match {
					case Left(p) =>
						db.addPredicate(p)
					case Right(a) =>
						db.addCoref(new ArgumentCoref(a))
				}
				mapping.add(mr.toMention, mr)
			})
			mapping
		}

		log("computing alignments and scores for %d document pairs...".format(docPairs.size))
		val alignments = new ArrayBuffer[AlignmentFileUtil.AlignmentRef]
		for(dp <- docPairs) {
			log(dp.toString)
			val report = docs(dp.reportId).deepCopy()
			val passage = docs(dp.passageId).deepCopy()
			val reportMapping = addTo(report)
			val passageMapping = addTo(passage)
			val da = ham.align(report, passage, domain)
			val predGrid = DocMetaAligner.predAlignmentGrid(report, passage)
			val argGrid = DocMetaAligner.argAlignmentGrid(report, passage)
			for((grid, kind) <- Seq((predGrid, "p"), (argGrid, "a"))) {
				for((row, rowIdx) <- grid.zipWithIndex) {
					for((a, colIdx) <- row.zipWithIndex) {
						val conf = if(da.sureAlignments.contains(a)) "sure"
							else if(da.possibleAlignments.contains(a)) "possible"
							else "not_aligned"
						val comment = "# %.4f".format(ham.score(a, report, passage, domain))

						// get back mention ids that were passed in
						val (rmId, pmId) = a match {
							case pa: PredicateAlignment =>
								val rmr: MentionFileUtil.MentionRef = reportMapping.getForwards(pa.reportPred.location)
								val pmr: MentionFileUtil.MentionRef = passageMapping.getForwards(pa.passagePred.location)
								(rmr.id, pmr.id)
							case aca: ArgCorefAlignment =>
								val rmr = reportMapping.getForwards(aca.reportCoref.head.location)
								val pmr = passageMapping.getForwards(aca.passageCoref.head.location)
								(rmr.id, pmr.id)
						}
						//val rmId = "%s-%s%d".format(report.id, kind, rowIdx)
						//val pmId = "%s-%s%d".format(passage.id, kind, colIdx)

						alignments += AlignmentFileUtil.AlignmentRef(dp.hitId, rmId, pmId, conf, Some(comment))
					}
				}
			}
		}

		AlignmentFileUtil.writeAlignmentsTo(outFile, alignments.toSeq)
	}
}


