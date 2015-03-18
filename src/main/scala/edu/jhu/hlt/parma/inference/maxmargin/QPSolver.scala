package edu.jhu.hlt.parma.inference.maxmargin

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import collection.mutable.ArrayBuffer
import java.io._

/**
 * deltaFeatures = f(z_gold) - f(z_hyp)
 */
case class QPConstraint(val deltaFeatures: DVec, val loss: Double) {
	require(loss > 0d)
	def hinge(weights: DVec): Double = {
		val deltaScore = VecOps.dot(deltaFeatures, weights)
		math.max(0d, loss + deltaScore)
	}
}

case class QP(val dimension: Int, val svmC: Double, val constraints: IndexedSeq[QPConstraint]) {
	require(svmC > 0d)
	require(dimension > 0)
	def objectiveValue(weights: DVec): Double = {
		val (cons, hinge) = constraints.map(c => (c, c.hinge(weights))).maxBy(_._2)
		val ww = VecOps.dot(weights, weights)
		ww + svmC * hinge
	}
	def objectiveGradient(weights: DVec): DVec = {
		val grad = new DVec(dimension)
		val (cons, hinge) = constraints.map(c => (c, c.hinge(weights))).maxBy(_._2)
		if(hinge > 0d)
			grad += (cons.deltaFeatures * svmC)
		grad += weights
		grad
	}
	def toFile(f: File) {
		val w = FileUtils.getWriter(f)
		w.write("dimension %d\n".format(dimension))
		w.write("svmC %f\n".format(svmC))
		for(c <- constraints)
			w.write(QPUtil.cons2str(c) + "\n")
		w.close
	}
}
object QP {
	def fromFile(f: File): QP = {
		val r = FileUtils.getReader(f)

		var ar = r.readLine.trim.split("\\s+")
		require(ar.length == 2 && ar(0) == "dimension")
		val dim = ar(1).toInt

		ar = r.readLine.trim.split("\\s+")
		require(ar.length == 2 && ar(0) == "svmC")
		val svmC = ar(1).toDouble

		val cons = new ArrayBuffer[QPConstraint]
		while(r.ready) {
			val line = r.readLine.trim
			cons += QPUtil.str2cons(line)
		}

		r.close
		QP(dim, svmC, cons.toIndexedSeq)
	}
}

object QPUtil {

	def vec2str(vec: DVec): String = vec.values.mkString(" ")

	def cons2str(c: QPConstraint): String =
		"%f %s".format(c.loss, vec2str(c.deltaFeatures))

	def str2cons(str: String): QPConstraint = {
		val ar = str.trim.split("\\s+")
		val loss = ar(0).toDouble
		val df = new DVec(ar.length - 1)
		var i = 1
		while(i < ar.length) {
			df(i-1) = ar(i).toDouble
			i += 1
		}
		QPConstraint(df, loss)
	}
}

class QPSolver(val problem: QP) {

	var w = new DVec(problem.dimension)

	def train {
		var done = false
		while(!done) {
			val g = problem.objectiveGradient(w)
			val decrease = lineSearch(g)
			require(decrease >= 0d)
			done = (decrease < 1e-5)
		}
	}

	/** returns how much it lowered the objective by */
	def lineSearch(direction: DVec): Double = {
		var alpha = 1d
		var done = false
		val f = problem.objectiveValue(w)
		var f_new = f
		while(alpha > 1e-9) {
			val w_new = w + (direction * (-alpha))	// min, step away from the gradient
			f_new = problem.objectiveValue(w_new)
			if(f_new * (1d + alpha/1000d) < f)	// TODO Wolfe conditions
				done = true
			else alpha /= 2
		}
		f - f_new
	}

}

