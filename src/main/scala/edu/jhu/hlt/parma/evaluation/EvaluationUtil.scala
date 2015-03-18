package edu.jhu.hlt.parma.evaluation

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.inference._

object EvaluationUtil {
	def evaluate(instances: Seq[Instance[DocAlignment]]): Seq[(String, Double)] = {
		for(f <- EvaluationFunction.allFunctions)
			yield (f.name, f(instances))
	}
	def evaluate(hyp: Seq[DocAlignment], gold: Seq[DocAlignment]): Seq[(String, Double)] = {
		require(hyp.size == gold.size)
		require(hyp.size > 0)
		evaluate(hyp.zip(gold).map(hg => Instance(hg._1, hg._2)))
	}
	def evaluate(ie: InferenceEngine[_], gold: Seq[DocAlignment]): Seq[(String, Double)] = {
		//val hyp = gold.map(da => ie.align(da.report, da.passage, da.domain))
		val hyp = ie.align(gold.map(_.report), gold.map(_.passage), gold.map(_.domain))
		evaluate(hyp, gold)
	}

	def checkInstances(instances: Seq[Instance[DocAlignment]]) {
		for(inst <- instances) {
			require(inst.gold.report eq inst.hyp.report)
			require(inst.gold.passage eq inst.hyp.passage)
		}
	}
}

