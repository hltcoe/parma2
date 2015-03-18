// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.diagnostics

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.inference._
import edu.jhu.hlt.parma.experiments.Pipeline
import java.io.{PrintWriter, File}

object FeatureDiagnostics extends Logging2 {

	val OUTPUT_DIR = "diagnostics.features.outdir"

   	def writeoutAlignmentFeatures(inst: Instance[DocAlignment], featureRep: FeatureRepresentation) {
		val od = ParmaConfig.getString(OUTPUT_DIR, null)
		if(od == null) return
		val odir = new File(od)
		if(!odir.isDirectory) return

		Profiler.startTask("FeatureDiagnostics:writeoutAlignmentFeatures")
		
		val fn = inst.gold.id.replace(System.getProperty("file.separator"), "_")
		log("writing out features for doc alignment " + inst.gold.id)
		val w = new PrintWriter(FileUtils.getWriter(new File(odir, fn)))

		writeoutAlignmentFeatures(inst, featureRep, w)

		w.close
		Profiler.endTask("FeatureDiagnostics:writeoutAlignmentFeatures")
	}

   	def writeoutAlignmentFeatures(inst: Instance[DocAlignment], featureRep: FeatureRepresentation, w: PrintWriter) {

		w.print("report = " + Describe.document(inst.gold.report))
		w.print("passage = " + Describe.document(inst.gold.passage))
		w.println()

		val engine = featureRep.controller
		val scores =
			if(featureRep.inspectScores.isEmpty) Map[Alignment, Double]()
			else featureRep.inspectScores.get
		
		// write out every alignment and its feature vector
		featureRep.inspectFeatures match {
			case None => w.write(engine.name + " provided no feature mapping for debugging\n")
			case Some(featMap) => {
				val parameters = engine.parameters
				for(((a, sv), idx) <- featMap.iterator.zipWithIndex) {

					val lab =
						if(inst.gold.sureAlignments.contains(a)) "POS+"
						else if(inst.gold.possibleAlignments.contains(a)) "POS~"
						else "NEG"
					val guess = if(inst.hyp.sureAlignments.contains(a)) "POS" else "NEG"
					val t = a match {
						case aca: ArgCorefAlignment => "ArgCoref"
						case pa: PredicateAlignment => "Predicate"
					}

					val (reportMention, passageMention) = CanonicalMentionFinder.canonicalMentions(a, inst.gold.report, inst.gold.passage)
					val reportSent = inst.gold.report.getSentence(reportMention)
					val passageSent = inst.gold.passage.getSentence(passageMention)
					w.write("========== Alignment %d label=%s guess=%s type=%s ==========\n".format(idx+1, lab, guess, t))
					w.write("report mention = " + Describe.mentionInContext(reportMention, inst.gold.report, 12) + "\n")
					w.write("passage mention = " + Describe.mentionInContext(passageMention, inst.gold.passage, 12) + "\n")
					w.println()
					w.write("alignment = " + Describe.alignment(a, inst.gold.report, inst.gold.passage))
					w.println()
					w.write("most influential features:\n")
					w.write(Describe.linearDecision(sv, parameters, engine.featureName, 20))
					scores.get(a) match {
						case Some(s) =>
							w.write("score = " + s)
							w.println()
						case None => {}
					}
					w.println()
					w.write("report sentence = " + Describe.sentence(reportSent))
					w.write("passage sentence = " + Describe.sentence(passageSent))
					w.println()
				}
			}
		}
		w.flush
	}
}

