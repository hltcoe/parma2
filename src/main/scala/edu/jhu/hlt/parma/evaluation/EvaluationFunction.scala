package edu.jhu.hlt.parma.evaluation

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._

trait EvaluationFunction {
	def name: String = getClass.getName.split("\\.").last.replace("$", "")
	def apply(instances: Seq[Instance[DocAlignment]]): Double

	/**
	 * if this is already a loss function (i.e. higher is worse), return this
	 * otherwise return a negated version of apply
	 */
	def asLossFunction: EvaluationFunction
}

trait EvaluationScore extends EvaluationFunction {
	override def asLossFunction: EvaluationLoss =
		EvaluationFunction.negate(this)
}
trait EvaluationLoss extends EvaluationFunction {
	override def asLossFunction: EvaluationLoss = this
}

object EvaluationFunction {

	def allFunctions: Seq[EvaluationFunction] =
		Seq(MicroF1, MicroPrecision, MicroRecall,
			MacroF1, MacroPrecision, MacroRecall,
			MicroPredPrecision, MicroPredRecall, MicroPredF1,
			MicroArgPrecision, MicroArgRecall, MicroArgF1,
			MacroPredPrecision, MacroPredRecall, MacroPredF1,
			MacroArgPrecision, MacroArgRecall, MacroArgF1,
			TruePositives, TrueNegatives, FalsePositives, FalseNegatives,
			MicroHamming, MacroHamming)

	def basic: Seq[EvaluationFunction] = Seq(MicroF1, MicroPrecision, MicroRecall, MacroHamming)

	def negate(f: EvaluationScore): EvaluationLoss = new EvaluationLoss {
		override def name: String = f.name + "-Negated"
		override def apply(instances: Seq[Instance[DocAlignment]]): Double = -f.apply(instances)
		override def asLossFunction: EvaluationLoss = throw new RuntimeException("don't do this")
	}
	def negate(f: EvaluationLoss): EvaluationScore = new EvaluationScore {
		override def name: String = f.name + "-Negated"
		override def apply(instances: Seq[Instance[DocAlignment]]): Double = -f.apply(instances)
		override def asLossFunction: EvaluationLoss = throw new RuntimeException("don't do this")
	}
}

// ========================= Counts ==========================
object TruePositives extends EvaluationScore {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double =
		instances.foldLeft(0.0)(_ + SetBasedEvaluator.truePos(_))
}
object TrueNegatives extends EvaluationScore {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double =
		instances.foldLeft(0.0)(_ + SetBasedEvaluator.trueNeg(_))
}
object FalsePositives extends EvaluationLoss {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double =
		instances.foldLeft(0.0)(_ + SetBasedEvaluator.falsePos(_))
}
object FalseNegatives extends EvaluationLoss {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double =
		instances.foldLeft(0.0)(_ + SetBasedEvaluator.falseNeg(_))
}


// ========================= Hamming =========================
object MicroHamming extends EvaluationLoss {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double = apply(instances, SetBasedEvaluator.All)
	def apply(instances: Seq[Instance[DocAlignment]], take: SetBasedEvaluator.AlignmentsToTake): Double = {
		val hams = instances.map(SetBasedEvaluator.hamming(_, normalize=false, take=take))
		hams.sum / hams.size
	}
}

object MacroHamming extends EvaluationLoss {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double = apply(instances, SetBasedEvaluator.All)
	def apply(instances: Seq[Instance[DocAlignment]], take: SetBasedEvaluator.AlignmentsToTake): Double = {
		val hams = instances.map(SetBasedEvaluator.hamming(_, normalize=true, take=take))
		hams.sum / hams.size
	}
}


// ========================= mIcro =========================
object MicroPrecision extends EvaluationScore {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double = apply(instances, SetBasedEvaluator.All)
	def apply(instances: Seq[Instance[DocAlignment]], take: SetBasedEvaluator.AlignmentsToTake): Double = {
		var num = 0d
		var denom = 0d
		for(i <- instances) {
			val (p, w) = SetBasedEvaluator.generousPrecisionWithWeight(i, take)
			assert(!p.isNaN)
			num += p * w
			denom += w
		}
		if(denom == 0) 1d	// didn't return any predictions
		else num / denom
	}
}

object MicroRecall extends EvaluationScore {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double = apply(instances, SetBasedEvaluator.All)
	def apply(instances: Seq[Instance[DocAlignment]], take: SetBasedEvaluator.AlignmentsToTake): Double = {
		var num = 0d
		var denom = 0d
		for(i <- instances) {
			val (r, w) = SetBasedEvaluator.generousRecallWithWeight(i, take)
			assert(!r.isNaN)
			num += r * w
			denom += w
		}
		if(denom == 0) 1d	// no alignments to return
		else num / denom
	}
}

object MicroF1 extends EvaluationScore {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double = apply(instances, SetBasedEvaluator.All)
	def apply(instances: Seq[Instance[DocAlignment]], take: SetBasedEvaluator.AlignmentsToTake): Double = {
		val p = MicroPrecision(instances, take)
		val r = MicroRecall(instances, take)
		if(p + r == 0d) 0d
		else 2d * p * r / (p + r)
	}
}

// ========================= mAcro =========================
object MacroPrecision extends EvaluationScore {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double = apply(instances, SetBasedEvaluator.All)
	def apply(instances: Seq[Instance[DocAlignment]], take: SetBasedEvaluator.AlignmentsToTake): Double = {
		val ps = instances.map(SetBasedEvaluator.generousPrecisionWithWeight(_, take)._1)
		if(ps.size == 0) {
			println("WARNING: " + name + " was called on 0 examples")
			0d
		}
		else ps.sum / ps.size
	}
}

object MacroRecall extends EvaluationScore {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double = apply(instances, SetBasedEvaluator.All)
	def apply(instances: Seq[Instance[DocAlignment]], take: SetBasedEvaluator.AlignmentsToTake): Double = {
		val ps = instances.map(SetBasedEvaluator.generousRecallWithWeight(_, take)._1)
		if(ps.size == 0) {
			println("WARNING: " + name + " was called on 0 examples")
			0d
		}
		else ps.sum / ps.size
	}
}

object MacroF1 extends EvaluationScore {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double = apply(instances, SetBasedEvaluator.All)
	def apply(instances: Seq[Instance[DocAlignment]], take: SetBasedEvaluator.AlignmentsToTake): Double = {
		val p = MacroPrecision(instances, take)
		val r = MacroRecall(instances, take)
		if(p + r == 0d) 0d
		else 2d * p * r / (p + r)
	}
}


// just preds
object MacroPredPrecision extends EvaluationScore {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double = MacroPrecision(instances, SetBasedEvaluator.Preds)
}
object MacroPredRecall extends EvaluationScore {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double = MacroRecall(instances, SetBasedEvaluator.Preds)
}
object MacroPredF1 extends EvaluationScore {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double = MacroF1(instances, SetBasedEvaluator.Preds)
}
object MicroPredPrecision extends EvaluationScore {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double = MicroPrecision(instances, SetBasedEvaluator.Preds)
}
object MicroPredRecall extends EvaluationScore {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double = MicroRecall(instances, SetBasedEvaluator.Preds)
}
object MicroPredF1 extends EvaluationScore {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double = MicroF1(instances, SetBasedEvaluator.Preds)
}


// just args
object MacroArgPrecision extends EvaluationScore {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double = MacroPrecision(instances, SetBasedEvaluator.Args)
}
object MacroArgRecall extends EvaluationScore {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double = MacroRecall(instances, SetBasedEvaluator.Args)
}
object MacroArgF1 extends EvaluationScore {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double = MacroF1(instances, SetBasedEvaluator.Args)
}
object MicroArgPrecision extends EvaluationScore {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double = MicroPrecision(instances, SetBasedEvaluator.Args)
}
object MicroArgRecall extends EvaluationScore {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double = MicroRecall(instances, SetBasedEvaluator.Args)
}
object MicroArgF1 extends EvaluationScore {
	override def apply(instances: Seq[Instance[DocAlignment]]): Double = MicroF1(instances, SetBasedEvaluator.Args)
}

