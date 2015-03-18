package edu.jhu.hlt.parma.inference.maxmargin

import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.inference.DocMetaAligner
import ilog.concert._
import ilog.cplex._
import java.util.Arrays
import collection.mutable.ArrayBuffer
import util.Random
import java.io.File

object CPGlobalAlignment {

	def cplexThreads: Int = ParmaConfig.getInt("inference.ssvm.cplex-threads", 1)

	// in seconds
	def cplexTrainTimeLimit: Double = ParmaConfig.getInt("inference.ssvm.decode-train-timeout") * 60
	def cplexTestTimeLimit: Double = ParmaConfig.getInt("inference.ssvm.decode-test-timeout") * 60

	/** adds a extra loop over all the cached constraints for every call to cplex */
	def debugMVCache: Boolean = ParmaConfig.getBoolean("inference.ssvm.debug-mv-cache", false)

	def debugSharingFactors: Boolean = ParmaConfig.getBoolean("inference.ssvm.debug-sharing-factors", false)

	def defaultMVCacheSize: Int = ParmaConfig.getInt("inference.ssvm.mv-cache-size")

	// do this for speed (lots of threads can make IO a bottle-neck)
	def suppressCplexOutput: Boolean = true
}

class TimeoutCounter(val name: String, val timeoutThresh: Double = 1d) {
	require(timeoutThresh <= 1d)
	private var runs = 0
	private var timeouts = 0
	def +=(otc: TimeoutCounter) {
		runs += otc.runs
		timeouts += otc.timeouts
		require(propTimeouts < timeoutThresh || numTimeouts < 5 + math.ceil(1d / timeoutThresh).toInt)
	}
	def timeout {
		timeouts += 1
		runs += 1
		require(propTimeouts < timeoutThresh || numTimeouts < 5 + math.ceil(1d / timeoutThresh).toInt)
	}
	def optimal { runs += 1 }
	def clear {
		runs = 0
		timeouts = 0
	}
	def numTimeouts: Int = timeouts
	def numRuns: Int = runs
	def propTimeouts: Double = timeouts / runs.toDouble
	override def toString: String =
		"(TimeoutCounter:%s %d of %d (%.1f%%) non-optimal)".format(name, timeouts, numRuns, 100d*timeouts/numRuns)
}

/**
 * keeps the capacity most recent items added
 */
class RollingCache[T](val capacity: Int) extends Iterable[T] {
	private val buf = new ArrayBuffer[T]
	private var top = 0

	/**
	 * always adds the item, but returns true
	 * if it had to evict an old element to do so
	 */
	def add(t: T): Boolean = {
		val evict = buf.size == capacity
		if(evict) buf(top) = t
		else buf += t
		top += 1
		if(top == capacity) top = 0
		evict
	}

	def clear {
		buf.clear
		top = 0
	}

	override def size: Int = buf.size

	override def iterator: Iterator[T] = buf.iterator
}

/**
 * represents a full alignment grid with factors that sits on top of cplex
 *
 * this is a stateful class, and you should call the methods roughly in the
 * order that they appear
 */
class CPGlobalAlignment(val cacheSize: Int = CPGlobalAlignment.defaultMVCacheSize) extends Logging2 {

	def debugState: String = {
		val sb = new StringBuilder
		sb.append("report=%s\n".format(report.id))
		sb.append("passage=%s\n".format(passage.id))
		sb.append("cacheSize=%d\n".format(cacheSize))
		sb.append("#predAlignments=%d\n".format(preds.size))
		sb.append("#argAlignments=%d\n".format(args.size))
		sb.append("#fert1=%d #fert2=%d\n".format(fert1.size, fert2.size))
		sb.append("#psa=%d #asp=%d\n".format(psaFactors.size, aspFactors.size))
		sb.append("#temp=%d\n".format(tempOrdFactors.size))
		sb.append("mvCache.size=%d\n".format(mvCache.size))
		sb.append("hasLoss=%s\n".format(if(lossFunc==null) "no" else "yes"))
		sb.append("decodeTimeouts=%s\n".format(decodeTimeouts))
		sb.append("mvTimeouts=%s\n".format(mvTimeouts))
		sb.append("avgAlignmentSize=%s\n".format(avgAlignmentSize))
		try { sb.append("constraintWeight=%f\n".format(constraintWeight)) }
		catch {
			case e: java.lang.AssertionError =>
				sb.append(e.getMessage)
		}
		for((a, i) <- preds.zipWithIndex)
			sb.append("\tpred(%d) sim=%f aligned=%s\n".format(i, a.sim, a.isAlignedStr))
		for((a, i) <- args.zipWithIndex)
			sb.append("\targ(%d) sim=%f aligned=%s\n".format(i, a.sim, a.isAlignedStr))
		sb.append("loss=%s\n".format(lossFunc))
		sb.toString
	}

	def id: String =
		if(report == null || passage == null) "CPGlobalAlignment[null]"
		else "CPGlobalAlignment[%s~%s]".format(report.id, passage.id)

	var verbose = true

	var cplex: IloCplex = null
	var objective: CPObjective = null

	var context: Context = null
	val preds = new ArrayBuffer[CPAlignment[PredicateAlignment]]
	val args = new ArrayBuffer[CPAlignment[ArgCorefAlignment]]

	val fert1 = new ArrayBuffer[CPFertilityFactor1]
	val fert2 = new ArrayBuffer[CPFactor with HasSignature]

	val psaFactors = new ArrayBuffer[CPFactor with HasSignature]
	val aspFactors = new ArrayBuffer[CPFactor with HasSignature]

	val tempOrdFactors = new ArrayBuffer[TimeMLFactor]

	// for logging
	val decodeTimeouts = new TimeoutCounter("decode")
	val mvTimeouts = new TimeoutCounter("most-violated")

	// only non-null if there is a label
	var trueAlignment: DocAlignment = null
	var lossFunc: CPHammingFactor = null

	val salt = util.Random.nextInt

	/**
	 * stuff used for Constraint weighting
	 * we want to weight small alignments more and big alignments less
	 * because the way the constraints are formulated (in terms of deltaFeatures and loss)
	 * are proportional to the grid size (and number of quadratic factors).
	 * if you set inference.ssvm.macro-constraint-weighting = 0, you are dividing out the size
	 * and all alignments are treated equal (which is good for macro statistics, but not micro)
	 */
	private var avgAlignmentSize: Option[Double] = None
	def setAvgAlignmentSize(size: Double) { avgAlignmentSize = Some(size) }
	def alignmentSize: Double = preds.size + args.size
	def constraintWeight: Double = {
		if(ParmaConfig.getBoolean("inference.ssvm.smart-weights")) {
			val lam = ParmaConfig.getDouble("inference.ssvm.macro-constraint-weighting")
			require(lam >= 0d)
			avgAlignmentSize match {
				case Some(v) =>
					// bigger alignments are weighted more, unless lam is big
					(this.alignmentSize + lam) / (v + lam)
				case None =>
					warn("you didn't set the average alignment size yet!")
					// if it makes a small difference, i'll let you get away with it
					assert(lam > 9d * this.alignmentSize, "lam=%f alignmentSize=%s".format(lam, this.alignmentSize))
					1d
			}
		}
		else 1d
	}

	/**
	 * does not include loss function
	 */
	def allFactors: Seq[CPFactor with HasSignature] =
		(preds ++ args ++ fert1 ++ fert2 ++ psaFactors ++ aspFactors ++ tempOrdFactors).toSeq
	
	def allFactorsIncludingLoss: Seq[CPFactor] = allFactors ++ Seq(lossFunc)

	/**
	 * used for scheduling parallel decoding
	 */
	def estimatedDecodeDifficulty: Double =
		allFactors.size.toDouble

	override def toString: String = {
		val sb = new StringBuilder
		sb.append("(CPGlobalAlignment\n")
		sb.append("preds:\n")
		for(p <- preds) sb.append("\t" + p + "\n")
		sb.append("args:\n")
		for(a <- args) sb.append("\t" + a + "\n")
		sb.append(")")
		sb.toString
	}

	def hasLabels: Boolean = {
		assert((trueAlignment == null) == (lossFunc == null))
		trueAlignment != null
	}

	/** how many alignments are in this grid? */
	def size: Int = preds.size + args.size

	def report = context.report
	def passage = context.passage
	def alignments: Seq[CPAlignment[_]] = (preds ++ args).toSeq

	/**
	 * use this if you have the labels (i.e. DocAlignment)
	 */
	def initData(featureFunc: (Alignment, Context) => SVec,
			da: DocAlignment, falseNegBias: Double) {
		trueAlignment = da
		val c = da.context
		val pas = DocMetaAligner.allPossiblePredAlignments(c).toSeq
		val acas = DocMetaAligner.allPossibleArgCorefAlignments(c).toSeq
		initData(c, pas, acas, Some(da.possibleAlignments), featureFunc)
		lossFunc = new CPHammingFactor(preds, args, falseNegBias)
		lossFunc.redirectLogTo(this)
	}

	/**
	 * use this if you don't have the labels (i.e. DocAlignment)
	 */
	def initData(c: Context, featureFunc: (Alignment, Context) => SVec) {
		val preds = DocMetaAligner.allPossiblePredAlignments(c).toSeq
		val args = DocMetaAligner.allPossibleArgCorefAlignments(c).toSeq
		initData(c, preds, args, None, featureFunc)
	}

	private def initData(
			c: Context,
			possiblePredAlignments: Seq[PredicateAlignment],
			possibleArgAlignments: Seq[ArgCorefAlignment],
			trueAlignments: Option[Set[Alignment]],
			featureFunc: (Alignment, Context) => SVec) {

		Profiler.startTask("CPGlobalAlignment:initData")
		preds.clear
		args.clear
		context = c

		def lab(a: Alignment): Option[Boolean] = trueAlignments match {
			case Some(m) => Some(m.contains(a))
			case None => None
		}

		// unary/similarity factors
		val pGrid = new GridAlphabet[Predicate]
		for(pa <- possiblePredAlignments) {
			val (row, col) = pGrid.lookupIndex(pa.reportPred, pa.passagePred, add=true)
			val features = featureFunc(pa, c)
			preds += new CPAlignment(pa, lab(pa), features, row, col)
		}
		val aGrid = new GridAlphabet[ArgumentCoref]
		for(aa <- possibleArgAlignments) {
			val (row, col) = aGrid.lookupIndex(aa.reportCoref, aa.passageCoref, add=true)
			val features = featureFunc(aa, c)
			args += new CPAlignment(aa, lab(aa), features, row, col)
		}
		Profiler.endTask("CPGlobalAlignment:initData")
	}

	/**
	 * Note, fert1Preds can equal fert1Args, etc
	 */
	def addFertilityFactors(
			fert1params: ParamRefRange with Active,
			fert2params: ParamRefRange with Active,
			splitPredArgCosts: Boolean,
			includeRowSizeFeatures: Boolean,
			fert1Pruner: Pruner[CPFertilityFactor1, CPGlobalAlignment] = new UniformCutoffPruner[CPFertilityFactor1, CPGlobalAlignment](1000),
			fert2Pruner: Pruner[CPFactor, CPGlobalAlignment] = new DefaultFert2Pruner(1000)) {

		val featDim = 10
		require(fert1params.size == featDim)
		require(fert2params.size == featDim)

		val fert1Temp = new ArrayBuffer[CPFertilityFactor1]
		val fert2Temp = new ArrayBuffer[CPFactor]
		Profiler.startTask("CPGlobalAlignment:addFertilityFactors")
		for(aset <- List(preds, args)) {	// i can't believe this syntax is valid, so awesome
			for(view <- Seq[CPAlignment[_] => Int](_.row, _.col)) {
				val groups: Map[Int, IndexedSeq[CPAlignment[_]]] = aset.groupBy(view)
				for((rowColIdx, alignments) <- groups) {

					val fval = 1d
					val features = DVec.rep(0, featDim)
					features(0) = fval		// the centroid, or feature thats always on
					if(splitPredArgCosts) {
						val p = fval/10d	// the smaller, the more regularized this feature
						if(aset eq preds) features(1) = p
						else features(2) = p
					}
					if(includeRowSizeFeatures) {
						val p = 0.1d	// the smaller, the more regularized this feature
						val s = alignments.size
						if(s == 1) features(3) = p
						if(s == 2) features(4) = p
						if(s == 3 || s == 4) features(5) = p
						if(s == 4 || s == 5) features(6) = p
						if(s > 5 && s < 10) features(7) = p
						if(s >=10 && s < 15) features(8) = p
						if(s > 25) features(9) = p
					}

					if(alignments.size > 0 && fert1params.active)
						fert1Temp += new CPFertilityFactor1(alignments, features, fert1params)

					if(alignments.size > 1 && fert2params.active) {
						fert2Temp += (ParmaConfig.getString("inference.ssvm.fert2-mode") match {
							case "max" => new CPFertilityFactor2(alignments, features, fert2params)
							case "sum" => new CPFertilityFactor2Sum(alignments, features, fert2params)
							case x =>
								throw new RuntimeException("fert2-mode %s is unknown".format(x))
						})
					}

				}
			}
		}

		val fert1Pruned = fert1Pruner.prune(fert1Temp.toSeq, this)
		fert1 ++= fert1Pruned
		log("[addFertilityFactors] created %d fert1 factors and pruned it down to %d"
			.format(fert1Temp.size, fert1Pruned.size))

		val fert2Pruned = fert2Pruner.prune(fert2Temp.toSeq, this)
		fert2 ++= fert2Pruned.asInstanceOf[TraversableOnce[CPFactor with HasSignature]]
		log("[addFertilityFactors] created %d fert2 factors and pruned it down to %d"
			.format(fert2Temp.size, fert2Pruned.size))

		Profiler.endTask("CPGlobalAlignment:addFertilityFactors")
	}

	/**
	 * given predicate alignment p_ij
	 * find all argument coref alignment a_kl (where a_k and a_l are a set of arguments)
	 * s.t.
	 * 		\exists a_k' \in a_k s.t. a_k' \in deps(p_i)
	 *		\exists a_l' \in a_l s.t. a_l' \in deps(p_j)
	 */
	def argAlignmentsFor[T <: PredicateAlignment](predCPAlignment: T, debug: Boolean = false): Seq[CPAlignment[ArgCorefAlignment]] = {

		val rp = predCPAlignment.reportPred
		val pp = predCPAlignment.passagePred
		if(debug) {
			log("[argAlignmentsFor]")
			log("[argAlignmentsFor] rPred=" + Describe.predicate(rp, report))
			log("[argAlignmentsFor] pPred=" + Describe.predicate(pp, passage))
		}

		// arguments of a given predicate (on either side)
		val rArgs = argsForPred(rp, report).toSet[Argument]		// a_k = deps(p_i)
		val pArgs = argsForPred(pp, passage).toSet[Argument]	// a_l = deps(p_j)
		if(debug) {
			log("[argAlignmentsFor] rArgs = " + rArgs.toSeq.map(a => Describe.argument(a, report)).mkString(", "))
			log("[argAlignmentsFor] pArgs = " + pArgs.toSeq.map(a => Describe.argument(a, passage)).mkString(", "))
		}

		// which ArgCorefAlignments touch an argument from each of these sets?
		args.filter(argCPAlignment => {
			val rCoref = argCPAlignment.alignment.reportCoref
			val pCoref = argCPAlignment.alignment.passageCoref
			
			val rIntersect = rCoref.toSet[Argument] & rArgs		// \exists a_k' \in a_k
			val pIntersect = pCoref.toSet[Argument] & pArgs		// \exists a_l' \in a_l

			val keep = rIntersect.size > 0 && pIntersect.size > 0
			if(debug && keep) {
				log("[argAlignmentsFor] rIntersect = " + rIntersect.toSeq.map(a => Describe.argument(a, report)).mkString(", "))
				log("[argAlignmentsFor] pIntersect = " + pIntersect.toSeq.map(a => Describe.argument(a, passage)).mkString(", "))
				log("[argAlignmentsFor] keep? = " + keep)
			}
			keep
		})
	}

	def addSharingFactors(
			psaNewParams: ParamRefRange with Active,
			psaOldParams: ParamRefRange with Active,
			aspNewParams: ParamRefRange with Active,
			aspOldParams: ParamRefRange with Active,
			predC10: ParamRef with Active,
			predC01: ParamRef with Active,
			predC11: ParamRef with Active,
			argC10: ParamRef with Active,
			argC01: ParamRef with Active,
			argC11: ParamRef with Active) {

		Profiler.startTask("CPGlobalAlignment:addSharingFactors")
		val debug = CPGlobalAlignment.debugSharingFactors

		implicit def stripCP[T <: Alignment](cp: CPAlignment[T]): T = cp.alignment
		
		if(predC10.active || predC01.active || predC11.active || psaNewParams.active || psaOldParams.active) {
			for(predCPAlignment <- preds) {
				val argAlignments = argAlignmentsFor(stripCP(predCPAlignment))
				if(argAlignments.size > 0) {

					if(psaNewParams.active || psaOldParams.active) {
						val features = DVec.rep(0d, 4)
						features(0) = 2d / preds.size
						val p = 0.2 / preds.size
						if(argAlignments.size == 1) features(1) = p
						if(argAlignments.size == 2) features(2) = p
						if(argAlignments.size == 3) features(3) = p
						val psaAlt = new PredsShouldShareArgsAlt(predCPAlignment, argAlignments, psaNewParams, psaOldParams, features)
						psaFactors += psaAlt
					} else if(predC10.active || predC01.active || predC11.active) {

						require(false, "use the new method (CPRelatednessFactorAlt) because it has been upated")

						val psa = new PredsShouldShareArgs(predCPAlignment, argAlignments, predC10, predC01, predC11)
						//if(debug && hasLabels) log(SharingDebug.describePSA(psa, report, passage))
						psa.redirectLogTo(this)
						psaFactors += psa
					}

				}
			}
		}

		/**
		 * given an argument coref alignment a_kl (where a_k and a_l are a set of arguments)
		 * find all predicate alignments p_ij
		 * s.t.
		 * 		\exists a_k' \in a_k s.t. a_k' \in deps(p_i)
		 *		\exists a_l' \in a_l s.t. a_l' \in deps(p_j)
		 */
		def predAlignmentsFor(argAlignment: CPAlignment[ArgCorefAlignment]): Seq[CPAlignment[PredicateAlignment]] = {
			if(debug) {
				log("[predAlignmentsFor]")
				log("[predAlignmentsFor] given arg alignment: arg_coref_report = " + Describe.argCoref(argAlignment.alignment.reportCoref, report))
				log("[predAlignmentsFor] given arg alignment: arg_coref_passage = " + Describe.argCoref(argAlignment.alignment.passageCoref, passage))
				for((arg, idx) <- argAlignment.alignment.reportCoref.zipWithIndex)
					log("[predAlignmentsFor] full sentence: sent(arg_coref_report(%d)) = %s".format(idx, report.getSentence(arg)))
				for((arg, idx) <- argAlignment.alignment.passageCoref.zipWithIndex)
					log("[predAlignmentsFor] full sentence: sent(arg_coref_passage(%d)) = %s".format(idx, passage.getSentence(arg)))
			}
			val rPreds = argAlignment.alignment.reportCoref.flatMap(arg => predForArg(arg, report)).toSet	// p_i = { p_i' : \exits a_k and dep(a_k, p_i') }
			val pPreds = argAlignment.alignment.passageCoref.flatMap(arg => predForArg(arg, passage)).toSet	// p_j = { p_j' : \exits a_l and dep(a_l, p_j') }
			preds.filter(predCPAlignment => {
				val rp = predCPAlignment.alignment.reportPred
				val pp = predCPAlignment.alignment.passagePred
				val keep = rPreds.contains(rp) && pPreds.contains(pp)	// \exists p_ij s.t. p_i' \in p_i and p_j' \in p_i
				if(debug) {
					log("[predAlignmentsFor] reportPred = " + Describe.predicate(rp, report))
					log("[predAlignmentsFor] passagePred = " + Describe.predicate(pp, passage))
					log("[predAlignmentsFor] predAligned=%s argAligned=%s".format(predCPAlignment.isAlignedStr, argAlignment.isAlignedStr))
					log("[predAlignmentsFor] keep? = " + keep)
				}
				keep
			})
		}
		if(argC10.active || argC01.active || argC11.active || aspNewParams.active || aspOldParams.active) {
			for(argCPAlignment <- args) {
				val predAlignments = predAlignmentsFor(argCPAlignment)
				if(predAlignments.size > 0) {

					if(aspNewParams.active || aspOldParams.active) {
						val features = DVec.rep(0d, 4)
						features(0) = 10d / args.size
						val p = 1d / args.size
						if(predAlignments.size == 1) features(1) = p
						if(predAlignments.size == 2) features(2) = p
						if(predAlignments.size == 3) features(3) = p
						val aspAlt = new ArgsShouldSharePredsAlt(argCPAlignment, predAlignments, aspNewParams, aspOldParams, features)
						aspFactors += aspAlt
					}
					else if(argC10.active || argC01.active || argC11.active) {

						require(false, "use the new method (CPRelatednessFactorAlt) because it has been upated")

						val f = new ArgsShouldSharePreds(argCPAlignment, predAlignments, argC10, argC01, argC11)
						f.redirectLogTo(this)
						aspFactors += f
					}

				}
			}
		}

		log("[addSharingFactors] #PSA=" + psaFactors.size)
		log("[addSharingFactors] #ASP=" + aspFactors.size)
		Profiler.endTask("CPGlobalAlignment:addSharingFactors")
	}

	// NOTE: used above
	// NOTE: normally this should only return one Predicate, but since we are working
	// with collapsed dependencies, sometimes we get more
	// e.g. "[he]_0 [murdered]_1 and was [sentenced]_2" => gov(2, 0) and gov(1, 0)
	private def predForArg(arg: Argument, doc: Document): Seq[Predicate] = {//Option[Predicate] = {
		val deps = doc.governs(arg.location).map(_.gov).toSet[Token]
		val poss = doc.predicates.filter(p =>
			deps.contains(doc.getHeadToken(p)))
		//log("[predForArg] sentence = " + doc.getSentence(arg))
		//log("[predForArg] argument = " + Describe.argument(arg, doc))
		//require(poss.size <= 1, "poss = "+ poss.map(Describe.predicate(_, doc)).mkString(", "))
		poss//.headOption
	}

	// NOTE: used above and below
	// special sauce
	// for now: use dependency parse to find arguments
	private def argsForPred(p: Predicate, d: Document): Seq[Argument] = {
		val debug = CPGlobalAlignment.debugSharingFactors
		val deps = d.governedBy(p.location).map(_.dep).toSet[Token]
		val args = d.arguments.filter(a => {
			val argHT = d.getHeadToken(a)
			deps.contains(argHT)
		})
		if(debug) {
			log("[argsForPred] sentence: " + d.getSentence(p))
			log("[argsForPred] predicate: " + Describe.mentionInContext(p.location, d, contextWordsEachSide=6))
			for((a, idx) <- args.zipWithIndex)
				log("[argsForPred] arg(%d): %s".format(idx, Describe.mentionInContext(a.location, d, contextWordsEachSide=1)))
			if(args.size == 0)
				log("[argsForPred] no arguments!")
			log("[argsForPred]")
		}
		args
	}

	import edu.jhu.hlt.parma.inference.temporal._
	def addFakeTemporalOrderingFactors(information: Double, tempOrdParam: ParamRef)(implicit rand: Random) {
		log("[addFakeTemporalOrderingFactors] adding fake factors, information=%.1f".format(information))
		val rOrdering = new SyntheticTemporalRelationPredictor(information, report.predicates, rand)
		val pOrdering = new SyntheticTemporalRelationPredictor(information, passage.predicates, rand)
		rOrdering.redirectLogTo(this)
		pOrdering.redirectLogTo(this)
		addTemporalOrderingFactors(rOrdering, pOrdering, tempOrdParam)
	}
	def addTSTemporalOrderingFactors(tempOrdParam: ParamRef) {
		log("[addRealTemporalOrderingFactors] adding time-sieve temporal factors")
		val rOrdering = new TimeSieveTLinks(report, report.predicates)
		val pOrdering = new TimeSieveTLinks(passage, passage.predicates)
		rOrdering.redirectLogTo(this)
		pOrdering.redirectLogTo(this)
		addTemporalOrderingFactors(rOrdering, pOrdering, tempOrdParam)
	}
	def addTemporalOrderingFactors(
			rOrdering: TemporalRelationPredictor, pOrdering: TemporalRelationPredictor, tempOrdParam: ParamRef,
			pruner: Pruner[TimeMLFactor, CPGlobalAlignment] = new ConfidenceTimeMLPruner(1000)) {

		val verbose = false

		warnIf(tempOrdFactors.size > 0,
			"[addTemporalOrderingFactors] are you sure you're not double adding temporal ordering factors? (there are %d already registered)"
				.format(tempOrdFactors.size))

		val alignmentMap: Map[(Predicate, Predicate), CPAlignment[PredicateAlignment]] = preds.map(pa =>
			((pa.alignment.reportPred, pa.alignment.passagePred), pa)).toMap

		var tries = 0
		for(a1 <- preds; a2 <- preds) {
			val x = a1.alignment.reportPred
			val y = a2.alignment.reportPred
			val a = a1.alignment.passagePred
			val b = a2.alignment.passagePred
			val diag1 = alignmentMap( (x,b) )
			val diag2 = alignmentMap( (y,a) )

			val rRel = rOrdering.relationship(x, y)
			val pRel = pOrdering.relationship(a, b)
			(rRel, pRel) match {
				case (Some(rr), Some(pr)) =>
					if(verbose) {
						val c = 10
						log("[addTemporalOrderingFactors] report predicates:")
						log(Describe.mentionInContext(x.location, report, contextWordsEachSide=c))
						log(Describe.mentionInContext(y.location, report, contextWordsEachSide=c))
						log("[addTemporalOrderingFactors] report predicates relation: " + rr)
						log("")
						log("[addTemporalOrderingFactors] passage predicates:")
						log(Describe.mentionInContext(a.location, passage, contextWordsEachSide=c))
						log(Describe.mentionInContext(b.location, passage, contextWordsEachSide=c))
						log("[addTemporalOrderingFactors] passage predicates relation: " + pr)
						log("")
					}
					tempOrdFactors += new TimeMLFactor(a1, a2, diag1, diag2, rr, pr, tempOrdParam)
				case _ => {}
			}
			tries += 1
		}
		val pruned = pruner.prune(tempOrdFactors.toSeq, this)
		log("[addTemporalOrderingFactors] %d temporal ordering factors (of %d possible), pruned down to %d"
			.format(tempOrdFactors.size, tries, pruned.size))
		tempOrdFactors.clear
		tempOrdFactors ++= pruned
	}

	class ConfidenceTimeMLPruner(val howMany: Int) extends Pruner[TimeMLFactor, CPGlobalAlignment] {
		override def prune(factors: Seq[TimeMLFactor], context: CPGlobalAlignment): Seq[TimeMLFactor] = {
			val keep = factors.sortBy(_.confidence).takeRight(howMany)
			println("[ConfidenceTimeMLPruner] given %d, keeping %d, willFire %d".format(factors.size, keep.size, keep.filter(_.willFire).size))
			keep
		}
	}


	def updateSimilarities(theta: DVec, offset: Double = 0d, debugFrom: String = null) {
		
		logIf(verbose && offset != 0d, "[CPGlobalAlignment updateSimilarities] offset=" + offset)

		preds.foreach(_.updateSimilarity(theta, offset))
		args.foreach(_.updateSimilarity(theta, offset))

		if(debugFrom != null) {
			val pp = preds.foldLeft(0)((s,a) => s + (if(a.sim > 0d) 1 else 0))
			val ap = args.foldLeft(0)((s,a) => s + (if(a.sim > 0d) 1 else 0))
			log("[updateSimilarities %s] %d of %d preds have sim>0, %d of %d args"
				.format(debugFrom, pp, preds.size, ap, args.size))
			def f(as: Seq[CPAlignment[_]]): String =
				as.map(a => {
					val l = if(a.hasLabel)
						if(a.isAligned) "+" else "-"
					else "?"
					"%+.1f:%s".format(a.sim, l)
				}).mkString(", ")
			log("[updateSimilarities %s] predSims=[%s] argSims=[%s]"
				.format(debugFrom, f(preds), f(args)))
		}
	}


	/** for debugging */
	def getDecodeCplex(theta: DVec, relax: Boolean = false): IloCplex = {
		updateSimilarities(theta, debugFrom=if(verbose) "decode" else null)
		cplex = new IloCplex()
		cplex.setParam(IloCplex.IntParam.Threads, CPGlobalAlignment.cplexThreads)
		cplex.setParam(IloCplex.DoubleParam.TiLim, CPGlobalAlignment.cplexTestTimeLimit)
		cplex.setParam(IloCplex.IntParam.MIPEmphasis, IloCplex.MIPEmphasis.Feasibility)
		cplex.setOut(if(CPGlobalAlignment.suppressCplexOutput) null else getLogger.getOutputStream)
		objective = new CPObjective(cplex, ParmaConfig.getBoolean("cplex.useQuadConstraints"))
		for(f <- allFactors)
			f.register(objective)
		objective.getObjective match {
			case Left(linearObj) =>
				logIf(verbose, "[CPGlobalAlignment decode] using linear objective with quadratic constraints")
				cplex.addMaximize(linearObj)
			case Right(quadObj) =>
				logIf(verbose, "[CPGlobalAlignment decode] using quadratic objective with no constraints")
				cplex.addMaximize(quadObj)
		}
		logIf(verbose, "[solve] about to call cplex, #pred=%d #args=%d #fert1=%d #fert2=%d #psaFactors=%d #aspFactors=%d"
			.format(preds.size, args.size, fert1.size, fert2.size, psaFactors.size, aspFactors.size, tempOrdFactors.size))

		if(relax) {
			// alignment variables are CPFactors
			var numRel = 0
			allFactors.foreach(_ match {
				case rf: RelaxableCPFactor =>
					numRel += 1
					rf.relax(cplex)
				case _ => {}
			})
			logIf(verbose, "[solve] relaxed %d of %d factors".format(numRel, allFactors.size))
		}

		// TODO call this method from decode
		cplex
	}
	

	/**
	 * this calls updateSimilarities for you.
	 * turn off profiling for parallel calls.
	 */
	def decode(theta: DVec, offset: Double = 0d, profile: Boolean = true, relax: Boolean = false): DocAlignment = {
		
		if(profile)
			Profiler.startTask("CPGlobalAlignment:decode")

		updateSimilarities(theta, offset, debugFrom=if(verbose) "decode" else null)
		cplex = new IloCplex()
		cplex.setParam(IloCplex.IntParam.Threads, CPGlobalAlignment.cplexThreads)
		cplex.setParam(IloCplex.DoubleParam.TiLim, CPGlobalAlignment.cplexTestTimeLimit)
		cplex.setParam(IloCplex.IntParam.MIPEmphasis, IloCplex.MIPEmphasis.Feasibility)
		cplex.setOut(if(CPGlobalAlignment.suppressCplexOutput) null else getLogger.getOutputStream)
		objective = new CPObjective(cplex, ParmaConfig.getBoolean("cplex.useQuadConstraints"))
		for(f <- allFactors)
			f.register(objective)
		objective.getObjective match {
			case Left(linearObj) =>
				logIf(verbose, "[CPGlobalAlignment decode] using linear objective with quadratic constraints")
				cplex.addMaximize(linearObj)
			case Right(quadObj) =>
				logIf(verbose, "[CPGlobalAlignment decode] using quadratic objective with no constraints")
				cplex.addMaximize(quadObj)
		}
		logIf(verbose, "[solve] about to call cplex, #pred=%d #args=%d #fert1=%d #fert2=%d #psaFactors=%d #aspFactors=%d"
			.format(preds.size, args.size, fert1.size, fert2.size, psaFactors.size, aspFactors.size, tempOrdFactors.size))

		if(relax) {
			// alignment variables are CPFactors
			var numRel = 0
			allFactors.foreach(_ match {
				case rf: RelaxableCPFactor =>
					numRel += 1
					rf.relax(cplex)
				case _ => {}
			})
			logIf(verbose, "[solve] relaxed %d of %d factors".format(numRel, allFactors.size))
		}

		cplex.solve()

		if(cplex.getStatus.toString != "Optimal") {
			warn("[decode] cplex did not find the optimal solution, status: " + cplex.getStatus)
			warn(debugState)
			decodeTimeouts.timeout
		}
		else decodeTimeouts.optimal

		val aligned = (preds ++ args).flatMap(a => {
			if(a.decode) Some(a.alignment)
			else None
		})
		logIf(verbose, "[solve] cplex objective = %.5f".format(cplex.getObjValue))
		cplex.end()
		val id = "CPG:r%s-p%s".format(report.id, passage.id)
		val domain = Some("CPG")
		if(profile)
			Profiler.endTask("CPGlobalAlignment:decode")
		new DocAlignment(id, domain, report, passage, aligned.toSet, Set())
	}

	/**
	 * cached calls to addMostViolatedConstraint
	 * reduces the number of calls to cplex
	 *
	 * NOTE: mvCache stores RAW delta features, which have not
	 * had their weight multiplied in yet (this is done when
	 * we call addTo.addAlignment in both the cached and uncached
	 * version of addMostViolatedConstraint).
	 * be careful with this!
	 */
	@transient
	val mvCache = new RollingCache[(DVec, Double, Int)](cacheSize)

	def flushCache { mvCache.clear }

	/**
	 * argmax_{y \in cache} theta * f(y) + loss(y)
	 *
	 * mvCache stores the gradient, which = f(y) - f(\hat{y})
	 * theta * f(y) is a constant w.r.t. theta
	 * (all elems of the cache have the same y -> same f(y) -> same theta * f(y))
	 * => just maximize w.r.t the negative of the gradience (hence "-1d * ...")
	 */
	def addMostViolatedConstraintCached(theta: DVec, addTo: Constraint, profile: Boolean = true) {
		if(profile)
			Profiler.startTask("CPGlobalAlignment:addMostViolatedConstraintCached")
		require(hasLabels)
		require(mvCache.size > 0, "are you sure CPLEX is linked? " +
			"this may be caused by not setting java.library.path to include the CPLEX executable directory. " +
			"check if there was any other Exception in this log file (which may occur even if CPLEX is linked). " +
			"if CPLEX is not the problem, good luck with debugging.")
		val (gradBuf, loss, sig) = mvCache.maxBy(dvl => dvl._2 - VecOps.dot(theta, dvl._1))
		addTo.addAlignment(gradBuf, loss, sig, this.constraintWeight)
		if(profile)
			Profiler.endTask("CPGlobalAlignment:addMostViolatedConstraintCached")
	}

	def fert2SumDebug(cplex: IloCplex) {
		val badFert2s = allFactors.flatMap(_ match {
			case b: CPFertilityFactor2Sum => Some(b)
			case _ => None
		}).toIndexedSeq
		
		val cons = new ArrayBuffer[IloConstraint]
		val consParent = new ArrayBuffer[Int]	// indices into badFert2s
		for((f2, idx) <- badFert2s.zipWithIndex) {
			val f2cons = f2.debuggingConstraints
			cons ++= f2cons
			consParent ++= f2cons.map(x => idx)
		}

		val consAr: Array[IloConstraint] = cons.toArray
		val success = cplex.refineConflict(consAr, consAr.map(x => 1d))
		require(success)
		val conflictStati: Array[IloCplex.ConflictStatus] = cplex.getConflict(consAr)
		var (bad, fine) = (0, 0)
		for((conflict, parentIdx) <- conflictStati.zip(consParent)) {
			val f2 = badFert2s(parentIdx)
			if(conflict != IloCplex.ConflictStatus.Excluded) {
				log("[fert2SumDebug] %s has conflict status %s".format(f2, conflict))
				bad += 1
			}
			else fine += 1
		}
		log("[fert2SumDebug] %d of %d were bad".format(bad, fine))

		val f = new File("/home/hltcoe/twolfe/fall2013/parma/diagnostics/cplex-bad-ilp/%s-%d.lp"
			.format(this.id, System.currentTimeMillis))
		cplex.exportModel(f.getPath)
	}

	/**
	 * cplex solves:
	 * argmax_{y \in 2^{m*n}} theta * f(y) + loss(y)
	 *
	 * if many threads, this blows up => profile = false
	 */
	def addMostViolatedConstraint(theta: DVec, addTo: Constraint, relax: Boolean = false, profile: Boolean = true, timeLimitMultiplier: Double = 1d) {

		if(profile)
			Profiler.startTask("CPGlobalAlignment:addMostViolatedConstraint")
		require(hasLabels)

		// compute similarities according to theta (stored as unary potentials in every CPAlignment)
		updateSimilarities(theta, debugFrom=if(verbose) "addMV" else null)

		// solve for most violated constraint
		val timeLimit = CPGlobalAlignment.cplexTrainTimeLimit * timeLimitMultiplier
		cplex = new IloCplex()
		cplex.setParam(IloCplex.IntParam.Threads, CPGlobalAlignment.cplexThreads)
		cplex.setParam(IloCplex.DoubleParam.TiLim, timeLimit)

		// i'm having some things come out as infeasible, and I'm fairly sure it is
		// due to timeouts rather than conflicting constraints
			// Balanced: Balance optimality and feasibility.
			// BestBound: Emphasize moving best bound.
			// Feasibility: Feasibility over optimality.
			// HiddenFeas: Emphasize finding hidden feasible solutions.
			// Optimality: Optimality over feasibility.
		// CONFIRMED: after setting a longer timeout (6 mins) and this setting, I don't get cplex errors here
		cplex.setParam(IloCplex.IntParam.MIPEmphasis, IloCplex.MIPEmphasis.Feasibility)

		cplex.setOut(if(CPGlobalAlignment.suppressCplexOutput) null else getLogger.getOutputStream)
		objective = new CPObjective(cplex, ParmaConfig.getBoolean("cplex.useQuadConstraints"))
		for(f <- allFactors)
			f.register(objective)
		lossFunc.register(objective)	// for separation oracle

		objective.getObjective match {
			case Left(linearObj) =>
				logIf(verbose, "[addMostViolatedConstraint] using linear objective, quadratic constraints")
				cplex.addMaximize(linearObj)
			case Right(quadObj) =>
				logIf(verbose, "[addMostViolatedConstraint] using quadratic objective")
				cplex.addMaximize(quadObj)
		}
		if(verbose) {
			log("[addMostViolatedConstraint] about to call cplex for da=[%s], #pred=%d #args=%d #fert1=%d #fert2=%d #psaFactors=%d #aspFactors=%d #tempFactors=%d"
				.format(this.id, preds.size, args.size, fert1.size, fert2.size, psaFactors.size, aspFactors.size, tempOrdFactors.size))
		}

		if(relax) {
			// alignment variables are CPFactors
			var numRel = 0
			allFactors.foreach(_ match {
				case rf: RelaxableCPFactor =>
					numRel += 1
					rf.relax(cplex)
				case _ => {}
			})
			logIf(verbose, "[solve] relaxed %d of %d factors".format(numRel, allFactors.size))
		}

		val start = System.currentTimeMillis
		try {
			val foundOpt = cplex.solve()
			warnIf(!foundOpt, "[mostViolatedConstraint] did not find the optima")
		} catch {
			case e: log.cplex.CpxException => 
				warn(debugState)
				warn("[mostViolatedConstraint] time limit was %.1f sec, took %.1f sec"
					.format(timeLimit, (System.currentTimeMillis-start)/1000d))
				warn("trying to find a feasible solution with a 10x time limit...")
				try { cplex.setParam(IloCplex.DoubleParam.TiLim, timeLimit * 10) }	// second chance
				catch {
					case e: Exception =>
						fert2SumDebug(cplex)
						throw e
				}
		}

		if(cplex.getStatus.toString != "Optimal") {
			warn("[mostViolatedConstraint] cplex did not find the optimal solution, status: " + cplex.getStatus)
			warn("[mostViolatedConstraint] time limit was " + timeLimit)
			warn(debugState)
			mvTimeouts.timeout
		}
		else mvTimeouts.optimal

		val gradBuf = DVec.rep(0d, theta.dimension)
		allFactors.foreach(_.accumGradient(gradBuf))
		val l = lossFunc()
		val s = signature
		addTo.addAlignment(gradBuf, l, s, this.constraintWeight)
		//log("gradBuf.bad=%s gradBuf.l2=%f loss=%f weight=%f"
		//	.format(gradBuf.containsBadValues(), gradBuf.l2, l, this.constraintWeight))
		//require(!addTo.containsBadValues())
		mvCache.add((gradBuf, l, s))
		if(verbose) log("[addMostViolatedConstraint] cplex objective=%.8g".format(cplex.getObjValue))

		// <DEBUGGING>
		if(CPGlobalAlignment.debugMVCache && mvCache.size > 0) {
			val scoreDebug = new CplexScoreDebug(theta, this)
			val justBuilt = new Constraint(theta.dimension)
			justBuilt.addAlignment(gradBuf, l, 0)//, this.constraintWeight)
			scoreDebug.verifyScores(justBuilt, cplex.getObjValue())

			val justFound = new Constraint(theta.dimension)
			justFound.addAlignment(gradBuf, l, s, this.constraintWeight)
			val bestInCache = new Constraint(theta.dimension)
			addMostViolatedConstraintCached(theta, bestInCache, profile)
			if(math.abs(justFound.hinge(theta) - bestInCache.hinge(theta)) > 1e-3) {
				for((cish, idx) <- mvCache.zipWithIndex) {
					val (g, l, s) = cish
					val c = new Constraint(theta.dimension)
					c.addAlignment(g, l, s, this.constraintWeight)
					log("cache(%d): %s hinge=%.8g".format(idx, c, c.hinge(theta)))
				}
				log("justFound: %s hinge=%.8g".format(justFound, justFound.hinge(theta)))
				log("bestInCache: %s hinge=%.8g".format(bestInCache, bestInCache.hinge(theta)))
				warn(debugState)

				// its unlikely that cplex missed the best solution by more than 10%, even under strict time constraints
				assert((bestInCache.hinge(theta) - justFound.hinge(theta)) / justFound.hinge(theta) < 0.10d)
				//assert(false)
			}
		}
		// </DEBUGGING>

		if(verbose) {
			def bs2s(bs: Seq[Boolean]): String = bs.map(if(_) "*" else "-").mkString
			log("[addMostViolatedConstraint] gold: preds=%s args=%s"
				.format(bs2s(preds.map(_.isAligned)), bs2s(args.map(_.isAligned))))
			log("[addMostViolatedConstraint] hyp:  preds=%s args=%s"
				.format(bs2s(preds.map(_.decode)), bs2s(args.map(_.decode))))
		}
		cplex.end()
		if(profile)
			Profiler.endTask("CPGlobalAlignment:addMostViolatedConstraint")
	}

	def signature: Int = {
		var sig = salt
		allFactors.foreach(sig ^= _.signature)
		sig
	}
}

