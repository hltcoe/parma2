// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.diagnostics

import edu.jhu.hlt.parma.evaluation._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import java.util.logging.Logger
import java.io.File

object BadAlignmentDiagnostics {
  
	val BAD_PRED_OUTFILE = "diagnostics.predictions.bad"
	
	val log = Logger.getLogger(this.getClass.getName)

	def isBad(da: Instance[DocAlignment]) = {
		val cutoff = 0.55
		val badP = SetBasedEvaluator.generousF1(da, take=SetBasedEvaluator.Preds) < cutoff
		val badA = SetBasedEvaluator.generousF1(da, take=SetBasedEvaluator.Args) < cutoff
		badP || badA
	}
	
	def printOutDiagnostics(alignmentInstances: Traversable[Instance[DocAlignment]]): Int = {
		val f = ParmaConfig.getString(BAD_PRED_OUTFILE, null)
		if(f == null) {
			log.info("if you want to log all of the bad predictions, specify a file in parma.config with " + BAD_PRED_OUTFILE)
			return -1
		}
		val badW = FileUtils.getWriter(new File(f))
		var n_bad = 0
		alignmentInstances.foreach(da => {
			if(isBad(da)) {
				n_bad += 1
				badW.write("bad prediction " + n_bad + " " + ("="*40) + "\n")
				badW.write(Describe.docAlignmentInstance(da))
				badW.newLine
				badW.newLine
			}
		})
		log.info("look in " + f + " for bad predictions")
		badW.close
		n_bad
	}
}


