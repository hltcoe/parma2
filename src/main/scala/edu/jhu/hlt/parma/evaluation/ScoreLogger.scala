package edu.jhu.hlt.parma.evaluation

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import collection.mutable.ArrayBuffer
import java.io.File

object ScoreLogger extends Logging2 {
	
	/**
	 * in order to do bootstrap confidence intervals after the fact,
	 * we need to log the scores seen on every instance.
	 */
	def logScores(f: File, instances: Seq[Instance[DocAlignment]]) {

		import SetBasedEvaluator.{ All, Preds, Args }
		implicit def d2s(d: Double): String = d.toString

		val w = FileUtils.getWriter(f)
		val lineBuf = new ArrayBuffer[String]
		for(inst <- instances) {
			lineBuf += inst.gold.id
			for(refinement <- Seq(All, Preds, Args)) {
				lineBuf += SetBasedEvaluator.generousPrecision(inst, refinement)
				lineBuf += SetBasedEvaluator.generousRecall(inst, refinement)
				lineBuf += SetBasedEvaluator.generousF1(inst, refinement)
			}
			w.write(lineBuf.mkString("\t") + "\n")
			lineBuf.clear
		}
		w.close
	}

	def logScores(wd: Option[WorkingDirectory], subsetName: String, instances: Seq[Instance[DocAlignment]]) {
		wd match {
			case None => log("not logging scores, no WorkingDirectory provided")
			case Some(w) =>
				val f = w.uniqFile(flags=Seq("instance-scores"), props=Map("subset"->subsetName))
				logScores(f, instances)
		}
	}
}

