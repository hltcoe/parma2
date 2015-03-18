package edu.jhu.hlt.parma.inference.maxmargin

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import ilog.concert._
import ilog.cplex._
import collection.mutable.ArrayBuffer

/**
 * a stateful factor (i.e. you do not pass in an assignment,
 * this maintains a reference to a stateful variable)
 */
trait CPFactor {

	/**
	 * add any constraints or terms in the objective required
	 */
	//def register(cplex: IloCplex, objective: IloLinearNumExpr)
	def register(cpObj: CPObjective)

	/**
	 * f(z) - f(\hat{z})
	 */
	def accumGradient(gradBuf: DVec)

	/**
	 * for debugging
	 */
	def getState(thetaDim: Int): CPFactorState
}

trait RelaxableCPFactor extends CPFactor {

	/**
	 * relax any variables that you created
	 */
	def relax(cplex: IloCplex)
}

abstract class CPRelatednessFactorAlt[T <: Alignment, R <: Alignment](
		val primary: CPAlignment[T],
		val related: Seq[CPAlignment[R]],
		val newParams: ParamRefRange,
		val oldParams: ParamRefRange,	// NOTE: ignored
		val features: DVec)
		extends RelaxableCPFactor with HasSignature with Logging2 {

	val tightRelaxation = false

	// primary  max(related)  factor
	// 1        0             1
	// 1        1             0
	// 0        1             0
	// 0        0             0
	// penalty >= primary - max(related)

	require(newParams.size == features.dimension)
	//require(oldParams.size == features.dimension)
	require(related.size > 0)

	private var cplex: IloCplex = null
	//private var newVar: IloIntVar = null
	//private var oldVar: IloIntVar = null
	//lazy val newVarGold = if(!primary.isAligned || related.filter(!_.isAligned).size > 0) 0d else 1d
	//lazy val oldVarGold = if(primary.isAligned && related.filter(_.isAligned).size > 0) 1d else 0d

	private var penVar: IloIntVar = null
	private var penVarGold = if(primary.isAligned && related.filter(_.isAligned).size == 0) 1d else 0d

	override def relax(cplex: IloCplex) {
		//log("[CPRelatednessFactorAlt] relaxing...")
		/*
		require(!tightRelaxation)
		if(newVar != null)
			cplex.add(cplex.conversion(newVar, IloNumVarType.Float))
		if(oldVar != null)
			cplex.add(cplex.conversion(oldVar, IloNumVarType.Float))
		*/
		cplex.add(cplex.conversion(penVar, IloNumVarType.Float))
	}

	override def register(cpObj: CPObjective) {
		//println("[CPRelatednessFactorAlt register] " + this)
		cplex = cpObj.cplex
		/*
		newVar = cplex.boolVar()
		oldVar = cplex.boolVar()
		for(a <- related) {
			val p = cplex.prod(primary.cplexVar, a.cplexVar)
			cplex.addLe(newVar, p)
			cplex.addGe(oldVar, p)
		}
		cpObj.linearTerm(getNewCost, newVar)
		cpObj.linearTerm(getOldCost, oldVar)
		*/
		penVar = cplex.boolVar
		//for(a <- related)
		//	cplex.addGe(penVar, cplex.sum(primary.cplexVar, a.cplexVar))
		val mVar = cplex.boolVar
		for(a <- related)
			cplex.addGe(mVar, a.cplexVar)
		cplex.addGe(penVar, cplex.sum(primary.cplexVar, cplex.prod(-1d, mVar)))
		cpObj.linearTerm(getPenalty, penVar)
	}

	//def getNewCost: Double = newParams.dot(features)
	//def getOldCost: Double = oldParams.dot(features)
	def getPenalty: Double = newParams.dot(features)
	
	override def accumGradient(gradBuf: DVec) {
		//val deltaNew: Double = newVarGold - cplex.getValue(newVar)
		//val deltaOld: Double = oldVarGold - cplex.getValue(oldVar)
		val delta: Double = penVarGold - cplex.getValue(penVar)
		val n = newParams.size
		var i = 0
		while(i < n) {
			//gradBuf(newParams.index(i)) += deltaNew * features(i)
			//gradBuf(oldParams.index(i)) += deltaOld * features(i)
			gradBuf(newParams.index(i)) += delta * features(i)
			i += 1
		}
	}

	override def getState(thetaDim: Int): CPFactorState = {
		//val newVarHyp = cplex.getValue(newVar)
		//val oldVarHyp = cplex.getValue(oldVar)
		val penVarHyp = cplex.getValue(penVar)
		//val costs = Array(getNewCost, getOldCost)
		//val vars = Array(newVarHyp, oldVarHyp)
		val costs = Array(getPenalty)
		val vars = Array(penVarHyp)
		val g = DVec.rep(0d, thetaDim)
		val h = DVec.rep(0d, thetaDim)
		val n = newParams.size
		var i = 0
		while(i < n) {
			//g(newParams.index(i)) += features(i) * newVarGold
			//h(newParams.index(i)) += features(i) * newVarHyp
			//g(oldParams.index(i)) += features(i) * oldVarGold
			//h(oldParams.index(i)) += features(i) * oldVarHyp
			g(newParams.index(i)) += features(i) * penVarGold
			h(newParams.index(i)) += features(i) * penVarHyp
			i += 1
		}
		CPFactorState(this, g, h, costs, vars, 0d)
	}

	override def signature: Int = 0
}

class PredsShouldShareArgsAlt(
		val predAlignment: CPAlignment[PredicateAlignment],
		val argAlignments: Seq[CPAlignment[ArgCorefAlignment]],
		override val newParams: ParamRefRange,
		override val oldParams: ParamRefRange,
		override val features: DVec)
		extends CPRelatednessFactorAlt(predAlignment, argAlignments, newParams, oldParams, features)

class ArgsShouldSharePredsAlt(
		val argAlignment: CPAlignment[ArgCorefAlignment],
		val predAlignments: Seq[CPAlignment[PredicateAlignment]],
		override val newParams: ParamRefRange,
		override val oldParams: ParamRefRange,
		override val features: DVec)
		extends CPRelatednessFactorAlt(argAlignment, predAlignments, newParams, oldParams, features)

/**
 * when we say cXY, we mean that X = binary variable for the primary
 * alignment, and Y is a binary variable representing the max (or) of
 * the related (or secondary) variables.
 * e.g. c10 means primary=1 and max(related)=0
 *
 * NOTE March 25: I haven't updated this, but I did the other one.
 */
abstract class CPRelatednessFactor[T <: Alignment, R <: Alignment](
		val primary: CPAlignment[T],
		val related: Seq[CPAlignment[R]],
		val c10: ParamRef with Active,
		val c01: ParamRef with Active,
		val c11: ParamRef with Active)
		extends RelaxableCPFactor with HasSignature with Logging2 {

	implicit def getIlo(cpa: CPAlignment[_]): IloIntVar = cpa.getCplexVar
	implicit def getIloS(cpas: Seq[CPAlignment[_]]): Seq[IloIntVar] = cpas.map(_.getCplexVar)

	require(related.size > 0)

	val debug = false

	val c10Coef = 1d
	val c01Coef = 1d
	val c11Coef = 1d

    private var objective: CPObjective = null
	private var cplex: IloCplex = null

	private var c10Var: IloIntVar = null
	private var c01Var: IloIntVar = null
	private var c11Var: IloIntVar = null

	lazy val c10Gold: Double = if(primary.isAligned && related.filter(_.isAligned).size == 0) 1d else 0d
	lazy val c01Gold: Double = if(!primary.isAligned && related.filter(_.isAligned).size > 0) 1d else 0d
	lazy val c11Gold: Double = if(primary.isAligned && related.filter(_.isAligned).size > 0) 1d else 0d

	def c10Hyp: Double = cplex.getValue(c10Var)
	def c01Hyp: Double = cplex.getValue(c01Var)
	def c11Hyp: Double = cplex.getValue(c11Var)

	override def relax(cplex: IloCplex) {
		log("[CPRelatednessFactor] relaxing...")
		if(c10Var != null)
			cplex.add(cplex.conversion(c10Var, IloNumVarType.Float))
		if(c01Var != null)
			cplex.add(cplex.conversion(c01Var, IloNumVarType.Float))
		if(c11Var != null)
			cplex.add(cplex.conversion(c11Var, IloNumVarType.Float))
	}

	override def register(cpObj: CPObjective) {

		if(!(c10.active || c01.active || c11.active)) {
			warn("[%s] should never have added this factor, no weights are active"
				.format(getClass.getName))
			return
		}

		objective = cpObj
		cplex = cpObj.cplex

		if(c10.active) {
			c10Var = cplex.boolVar()
			for(a <- related) {
				// c10 >= p * (1 - r_i) \forall i
				// c10 >= p - p * r_i   \forall i
				cplex.addGe(c10Var, cplex.sum(primary, cplex.negative(cplex.prod(primary, a))))
			}
			cpObj.linearTerm(c10Cost, c10Var)
		}
		if(c01.active) {
			c01Var = cplex.boolVar()
			val notP = cplex.sum(1d, cplex.negative(primary))
			for(a <- related)
				cplex.addGe(c01Var, cplex.prod(notP, a))
			cpObj.linearTerm(c01Cost, c01Var)
		}
		if(c11.active) {
			c11Var = cplex.boolVar()
			for(a <- related)
				cplex.addGe(c11Var, cplex.prod(primary, a))
			cpObj.linearTerm(c11Cost, c11Var)
		}
	}

	// theta
	// features = 1
	// coef
	// variables

	def c10Cost: Double = c10Coef * c10.value
	def c01Cost: Double = c01Coef * c01.value
	def c11Cost: Double = c11Coef * c11.value
	
	override def accumGradient(gradBuf: DVec) {
		if(c10.active)
			gradBuf(c10.index) += c10Coef * (c10Gold - c10Hyp)
		if(c01.active)
			gradBuf(c01.index) += c01Coef * (c01Gold - c01Hyp)
		if(c11.active)
			gradBuf(c11.index) += c11Coef * (c11Gold - c11Hyp)
	}

	override def getState(thetaDim: Int): CPFactorState = {
		val costs = Array.ofDim[Double](3)
		val vars = Array.ofDim[Double](3)
		val g = DVec.rep(0d, thetaDim)
		val h = DVec.rep(0d, thetaDim)
		if(c10.active) {
			g(c10.index) += c10Coef * c10Gold
			h(c10.index) += c10Coef * c10Hyp
			costs(0) = c10Cost
			vars(0) = c10Hyp
		}
		if(c01.active) {
			g(c01.index) += c01Coef * c01Gold
			h(c01.index) += c01Coef * c01Hyp
			costs(1) = c01Cost
			vars(1) = c01Hyp
		}
		if(c11.active) {
			g(c11.index) += c11Coef * c11Gold
			h(c11.index) += c11Coef * c11Hyp
			costs(2) = c11Cost
			vars(2) = c11Hyp
		}
		CPFactorState(this, g, h, costs, vars, 0d)
	}

	// this factor has no variables that are not a function of the existing alignment variables
	override def signature: Int = 0
}

/**
 * helps over nothing, but does not improve over fertility
 */
class PredsShouldShareArgs(
		val predAlignment: CPAlignment[PredicateAlignment],
		val argAlignments: Seq[CPAlignment[ArgCorefAlignment]],
		override val c10: ParamRef with Active,
		override val c01: ParamRef with Active,
		override val c11: ParamRef with Active)
		extends CPRelatednessFactor(predAlignment, argAlignments, c10, c01, c11)

/**
 * doesn't seem to help
 */
class ArgsShouldSharePreds(
		val argAlignment: CPAlignment[ArgCorefAlignment],
		val predAlignments: Seq[CPAlignment[PredicateAlignment]],
		override val c10: ParamRef with Active,
		override val c01: ParamRef with Active,
		override val c11: ParamRef with Active)
		extends CPRelatednessFactor(argAlignment, predAlignments, c10, c01, c11)


/**
 * falsePosPenalty for both preds and args is fixed at 1
 * falseNegPenalty is a multiple for negative instances,
 * which by default are weighted so that all of the hamming
 * loss possible on the positive examples is equal to that on
 * the negative examples
 */
class CPHammingFactor(
		val predAlignments: Seq[CPAlignment[PredicateAlignment]],
		val argAlignments: Seq[CPAlignment[ArgCorefAlignment]],
		val falseNegBias: Double)
		extends CPFactor
		with Logging2 {
	
	private var cplex: IloCplex = null

	val debug = false

	// PENALTIES >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
	require(falseNegBias > 0d)
	val lossMult = ParmaConfig.getDouble("inference.ssvm.loss-mult")
	val smooth = ParmaConfig.getDouble("inference.ssvm.loss-fn-smooth")
	log("[CPHammingFactor] falseNegSmooth=%f falseNegBias=%f lossMult=%f"
		.format(smooth, falseNegBias, lossMult))
	require(lossMult > 0d)
	require(smooth > 0d, "you could get nPos or nNeg equal 0")

	/**
	 * computes falseNeg and falsePos penalties based on a reasonable heuristic
	 *
	 * consider a constraint composed of a single alignment.
	 *   xi >= w * deltaFeatures(z,~z) + loss(z,~z)
	 *
	 * this can be decomposed into terms based on z_ij
	 *   xi >= w * deltaFeatures(z,~z) + loss_{FN}(z,~z) \forall z_ij=1
	 *   xi >= w * deltaFeatures(z,~z) + loss_{FP}(z,~z) \forall z_ij=0
	 *
	 * terms in the second camp are far more common (most alignments are negative).
	 * if deltaFeatures and loss are all getting summed up into one constraint, then
	 * we want to give more weight to the deltaFeatures on the less-common alignments
	 * if we think that false positives and negatives are equally important.
	 * so you might propose:
	 *   c_{FP} = 1 / (#z_ij=0)
	 *   c_{FN} = 1 / (#z_ij=1)
	 * because c_{FP} will show up #z_ij=0 times in the summed up constraint on xi
	 * and c_{FN} will show up #z_ij=1 times in the constraint.
	 *
	 * this function uses this heuristic with smoothing, and a final multiplier
	 * that may be applied to c_{FN} if it is too high or low (checked by a user
	 * after the fact).
	 *
	 * TODO if you really want to do a good job at optimizing F1, you can always
	 * do a locally linear approximation of that function using c_{FN|FP}.
	 * this would require knowing the counds of TP,FN,FP before hand, and doing
	 * some calculus.
	 */
	def penalties(alignments: Seq[CPAlignment[_]]): (Double, Double) = {

		val n = alignments.size
		val nPos = alignments.filter(_.isAligned).size
		val nNeg = alignments.size - nPos

		/*
		val falsePos = (1d + smooth) / (nNeg + smooth)
		val falseNeg = (1d + smooth) / (nPos + smooth)
		*/
		val falsePos = (n + smooth) / (nNeg + smooth)
		val falseNeg = (n + smooth) / (nPos + smooth)

		val sqFnBias = math.sqrt(falseNegBias)
		(lossMult * falsePos / sqFnBias, lossMult * falseNeg * sqFnBias)
	}

	/*
	 * ok, i think i goofed.
	 * instead of making this d / N, where N can grow,
	 * make it d * w_N where w_N is monotone in N, but centered around 1
	 * ... put another way, fnPen and fpPen should be proportional to the
	 *  _proportion_ of neg to pos alignments, but no the _number_ of alignments.
	 */

	def interp(a: (Double, Double), b: (Double, Double), lambdaA: Double): (Double, Double) = {
		require(lambdaA >= 0d && lambdaA <= 1d)
		val one = lambdaA * a._1 + (1d-lambdaA) * b._1
		val two = lambdaA * a._2 + (1d-lambdaA) * b._2
		(one, two)
	}

	val sqFnBias = math.sqrt(falseNegBias)
	val smartWeights = ParmaConfig.getBoolean("inference.ssvm.smart-weights")
	//require(!smartWeights, "are you SURE you want smart weights?")
	lazy val fullPenalties = penalties(predAlignments ++ argAlignments)	// if all you cared about about was the M[ai]croF1 column, you would use this
	val (falsePosPred, falseNegPred) =
		if(smartWeights) interp(penalties(predAlignments), fullPenalties, 0.8d)
		else (lossMult / sqFnBias, lossMult * sqFnBias)
	val (falsePosArg, falseNegArg) =
		if(smartWeights) interp(penalties(argAlignments), fullPenalties, 0.8d)
		else (lossMult / sqFnBias, lossMult * sqFnBias)

	require(falsePosPred > 0d && falsePosArg > 0d)
	require(falseNegPred > 0d && falseNegArg > 0d)


	// COUNTS >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
	val numPosPred: Int = predAlignments.filter(_.isAligned).size
	val numNegPred: Int = predAlignments.size - numPosPred
	val numPosArg: Int = argAlignments.filter(_.isAligned).size
	val numNegArg: Int = argAlignments.size - numPosArg

	def numFalsePosPred: Double = predAlignments.foldLeft(0d)((sum, a) => sum + (if(!a.isAligned) cplex.getValue(a.getCplexVar) else 0d))
	def numFalseNegPred: Double = predAlignments.foldLeft(0d)((sum, a) => sum + (if(a.isAligned) 1d - cplex.getValue(a.getCplexVar) else 0d))
	def numFalsePosArg: Double = argAlignments.foldLeft(0d)((sum, a) => sum + (if(!a.isAligned) cplex.getValue(a.getCplexVar) else 0d))
	def numFalseNegArg: Double = argAlignments.foldLeft(0d)((sum, a) => sum + (if(a.isAligned) 1d - cplex.getValue(a.getCplexVar) else 0d))


	override def toString: String = {
		val predPos: String =
			if(predAlignments.size == 0) "0"
			else if(predAlignments.head.hasLabel) predAlignments.filter(_.isAligned).size.toString
			else "?"
		val argPos: String =
			if(argAlignments.size == 0) "0"
			else if(argAlignments.head.hasLabel) argAlignments.filter(_.isAligned).size.toString
			else "?"
		"(CPHammingFactor predA=%s/%d #argA=%s/%d predFN/FP=%.2f/%.2f argFN/FP=%.2f/%.2f fnBias=%.2f svmD=%.2f)"
			.format(predPos, predAlignments.size, argPos, argAlignments.size, falseNegPred, falsePosPred, falseNegArg, falsePosArg, falseNegBias, lossMult)
	}

	/**
	 * note that the sign seems backwards: we are adding these factors
	 * so that we can find something with *high* loss, i.e. argmax_{y'} w*f(y') + loss(y',y)
	 */
	override def register(cpObj: CPObjective) {

		if(debug) log("[CPHammingFactor register] " + this.toString)

		cplex = cpObj.cplex

		// since we are subtracting off falseNegs, we will start by assuming
		// that we got them all wrong
		cpObj.addConstant(numPosPred * falseNegPred)
		cpObj.addConstant(numPosArg * falseNegArg)

		for(pa <- predAlignments) {
			// this is aligned => if you say yes you lost the opportunity to have a false neg
			if(pa.isAligned)
				cpObj.linearTerm(-falseNegPred, pa.getCplexVar)
			else
				cpObj.linearTerm(falsePosPred, pa.getCplexVar)
		}
		for(aa <- argAlignments) {
			if(aa.isAligned)
				cpObj.linearTerm(-falseNegArg, aa.getCplexVar)
			else
				cpObj.linearTerm(falsePosArg, aa.getCplexVar)
		}
	}

	/**
	 * there are no parameters corresponding to
	 * the loss factor, it is only used at train
	 * time to generate the margin constraints
	 */
	override def accumGradient(gradBuf: DVec) {
		warn("loss function has no gradient to accum!")
	}

	/**
	 * returns an un-normalized (w.r.t. alignments.size -- not an avg hamming loss)
	 * and weighted (according to falsePosPenalty and falseNegPenalty) hamming loss
	 */
	def loss: Double = {
		var l = 0d
		l += numPosPred * falseNegPred
		for(pa <- predAlignments.filter(_.decode)) {
			if(pa.isAligned) l += -falseNegPred
			else l += falsePosPred
		}
		l += numPosArg * falseNegArg
		for(aa <- argAlignments.filter(_.decode)) {
			if(aa.isAligned) l += -falseNegArg
			else l += falsePosArg
		}
		l
	}

	def apply() = loss

	override def getState(thetaDim: Int): CPFactorState = {
		val g = DVec.rep(0d, thetaDim)
		val h = DVec.rep(0d, thetaDim)

		val costs = Array(falsePosPred, falseNegPred, falsePosArg, falseNegArg)
		val vars = Array(numFalsePosPred, numFalseNegPred, numFalsePosArg, numFalseNegArg)

		val s = CPFactorState(this, g, h, costs, vars, loss)
		log("[CPHammingFactor getState] " + this)
		log("[CPHammingFactor getState] loss=" + loss)
		log("[CPHammingFactor getState] costs=" + costs.mkString(", "))
		log("[CPHammingFactor getState] vars=" + vars.mkString(", "))
		require(math.abs(loss - s.ilpScore) < 1e-3, "ilpScore=%f loss=%f".format(s.ilpScore, loss))
		s
	}
}

class CPFertilityFactor1(
		val alignments: IndexedSeq[CPAlignment[_]],
		val features: DVec,
		val params: ParamRefRange)
		extends RelaxableCPFactor with HasSignature with Logging2 {

	require(alignments.size > 0)

	val debug = false

    protected var cplex: IloCplex = null
    protected var fert1: IloIntVar = null

	override def relax(cplex: IloCplex) {
		require(fert1 != null)
		cplex.add(cplex.conversion(fert1, IloNumVarType.Float))
	}

	// this factor is used regardless of if the alignments have labels
	// (i.e. you can call a.isAligned) -- need to gaurd against this
	// ===> this is currently solved by making this a lazy val (seems to work...)
	private[this] lazy val f1gold = if(alignments.filter(_.isAligned).size > 0) 1d else 0d
	def f1hyp = cplex.getValue(fert1)

	def name: String = alignments.map(a => "a[%d,%d]".format(a.row, a.col)).mkString("~")

	override def toString: String =
		"(CPFertFactor1 #a=%d f1=%f)".format(alignments.size, getCost)

	override def register(cpObj: CPObjective) {
		logIf(debug, "[fert1 register] " + name + " cost=" + getCost)
		fert1 = cpObj.linearTermWithMax(getCost, alignments.map(_.getCplexVar))
		cplex = cpObj.cplex
    }

	override def accumGradient(gradBuf: DVec) {
		val n = params.size
		assert(n == features.dimension)
		var i = 0
		while(i < n) {
			gradBuf(params.index(i)) += (f1gold - f1hyp) * features(i)
			i += 1
		}
	}

	def getCost: Double = params.dot(features)

	override def getState(thetaDim: Int): CPFactorState = {
		val g = DVec.rep(0d, thetaDim)
		val h = DVec.rep(0d, thetaDim)
		var i = 0
		while(i < features.dimension) {
			g(params.index(i)) += features(i) * f1gold
			h(params.index(i)) += features(i) * f1hyp
			i += 1
		}
		CPFactorState(this, g, h, Array(getCost), Array(cplex.getValue(fert1)), 0d)
	}

	override def signature: Int = 0
}


/**
 * penalizes the max of the z_{ij} * z_{ik} product variables
 */
class CPFertilityFactor2(
		val alignments: IndexedSeq[CPAlignment[_]],
		val features: DVec,
		val params: ParamRefRange)
		extends RelaxableCPFactor with HasSignature with Logging2 {

	val debug = false

	val tightRelaxation = false

	require(alignments.size > 1)

	// let the features handle the coef
	// keep this as a placeholder in case I change my mind
	val coef = 1d

    private[this] var cplex: IloCplex = null
    private[this] var fert2: IloIntVar = null
	private[this] lazy val f2gold = if(alignments.filter(_.isAligned).size > 1) 1d else 0d
	def f2hyp = cplex.getValue(fert2)

	def getCost: Double = params.dot(features) * coef

	def name: String = alignments.map(a => "a[%d,%d]".format(a.row, a.col)).mkString("~")

	override def toString: String = "(CPFertFactor2 #a=%d coef=%f features=%s cost=%f)"
		.format(alignments.size, coef, features, getCost)

	override def relax(cplex: IloCplex) {
		require(fert2 != null)
		require(!tightRelaxation)
		cplex.add(cplex.conversion(fert2, IloNumVarType.Float))
	}
	
	override def register(cpObj: CPObjective) {
		logIf(debug, "[fert2 register] " + name + " cost=" + getCost)
		cplex = cpObj.cplex		// TODO consider adding this to CPObjective
		fert2 = cplex.boolVar()
		val idx = 0 until alignments.size
		for(a1 <- idx; a2 <- idx; if a1 < a2) {
			val aa1 = alignments(a1).getCplexVar
			val aa2 = alignments(a2).getCplexVar
			if(tightRelaxation)
				cplex.addGe(fert2, cplex.prod(aa1, aa2))
			else
				cplex.addGe(fert2, cplex.sum(-1, cplex.sum(aa1, aa2)))
		}
		cpObj.linearTerm(getCost, fert2)
    }

	override def accumGradient(gradBuf: DVec) {
		val f2hyp_alt = if(alignments.filter(_.decode).size > 1) 1d else 0d
		if(math.abs(f2hyp - f2hyp_alt) > 1e-3 && getCost < 0d) {
			warn("i think cplex messed up:")
			warn("decode = " + alignments.map(_.decode).mkString(", "))
			warn("f2penalty = %.8g".format(getCost))
			warn("f2hyp = %.8g cplex = %.8g".format(f2hyp, cplex.getValue(fert2)))
			assert(false)
		}
		val n = params.size
		assert(n == features.dimension)
		var i = 0
		while(i < n) {
			gradBuf(params.index(i)) += features(i) * coef * (f2gold - f2hyp)
			i += 1
		}
	}

	override def getState(thetaDim: Int): CPFactorState = {
		val g = DVec.rep(0d, thetaDim)
		val h = DVec.rep(0d, thetaDim)
		var i = 0
		while(i < features.dimension) {
			g(params.index(i)) += features(i) * coef * f2gold
			h(params.index(i)) += features(i) * coef * f2hyp
			i += 1
		}
		CPFactorState(this, g, h, Array(getCost), Array(cplex.getValue(fert2)), 0d)
	}

	override def signature: Int = 0
}

/**
 * penalizes each of the z_{ij} * z_{ik} product variables
 */
class CPFertilityFactor2Sum(
		val alignments: IndexedSeq[CPAlignment[_]],
		val features: DVec,
		val params: ParamRefRange)
		extends CPFactor with HasSignature with Logging2 {

	require(false, "this class has not been updated with the proper relaxation")
	require(alignments.size > 1)

    private[this] var cplex: IloCplex = null
	private[this] val fert2s = new ArrayBuffer[IloIntVar]
	private[this] var f2gold = Double.NaN

	// make this null to turn off debugging
	val debuggingConstraints = new ArrayBuffer[IloConstraint]

	def f2hyp = fert2s.map(cplex.getValue).sum

	// normalize by the number of pairs that might occur
	val coef = 10d / (alignments.size * (alignments.size-1) / 2)

	def getCost: Double = params.dot(features) * coef

	override def toString: String = "(CPFertFactor2Sum #a=%d coef=%f features=%s cost=%f)"
		.format(alignments.size, coef, features, getCost)

	override def register(cpObj: CPObjective) {
		fert2s.clear
		if(debuggingConstraints != null) debuggingConstraints.clear
		f2gold = 0d
		cplex = cpObj.cplex		// TODO consider adding this to CPObjective
		val idx = 0 until alignments.size
		for(a1 <- idx; a2 <- idx; if a1 < a2) {
			val aa1 = alignments(a1).getCplexVar
			val aa2 = alignments(a2).getCplexVar
			val prod = cplex.boolVar()
			val cons = cplex.addGe(prod, cplex.prod(aa1, aa2))
			if(debuggingConstraints != null) debuggingConstraints += cons
			fert2s += prod
			cpObj.linearTerm(getCost, prod)
			if(alignments(a1).hasLabel && alignments(a1).isAligned && alignments(a2).isAligned)
				f2gold += 1d
		}
    }

	override def accumGradient(gradBuf: DVec) {
		val n = params.size
		assert(n == features.dimension)
		var i = 0
		while(i < n) {
			gradBuf(params.index(i)) += features(i) * coef * (f2gold - f2hyp)
			i += 1
		}
	}

	// theta
	// features
	// coef
	// variable

	override def getState(thetaDim: Int): CPFactorState = {
		val g = DVec.rep(0d, thetaDim)
		val h = DVec.rep(0d, thetaDim)
		var i = 0
		while(i < features.dimension) {
			g(params.index(i)) += features(i) * coef * f2gold
			h(params.index(i)) += features(i) * coef * f2hyp
			i += 1
		}
		val costs: Array[Double] = fert2s.map(x => getCost).toArray
		val vars: Array[Double] = fert2s.map(cplex.getValue).toArray
		val s = CPFactorState(this, g, h, costs, vars, 0d)
		log("[CPFert2Sum] costs=[%s]".format(costs.mkString(", ")))
		log("[CPFert2Sum] vars= [%s]".format(vars.mkString(", ")))
		log("[CPFert2Sum] s.ilpScore=%f".format(s.ilpScore))
		s
	}

	override def signature: Int = 0
}




