package edu.jhu.hlt.parma.experiments.acl2014

import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.input._
import edu.jhu.hlt.parma.features._
import edu.jhu.hlt.parma.evaluation._
import edu.jhu.hlt.parma.experiments._
import edu.jhu.hlt.parma.inference._
import edu.jhu.hlt.parma.inference.maxmargin._
import collection.mutable.ArrayBuffer
import java.io.File

// take some das, a ham, and an cpinfeng
// load params from some file
// show predictions where they differ

object DADiffer extends Logging2 {

	val loss = MicroF1.asLossFunction

	def getHAM: InferenceEngine[_] = {
		val ham = new HierarchicalAlignmentModule
		val f = new File("diagnostics/hamtr/eecb.ham.params")
		if(f.exists)
			ham.readParameters(f)
		else {
			val das = ConcreteDocAlignmentReader.EECB.getDocAlignments
			val (train, dev) = DocAlignmentCorpus.randomSplit(das, 0.8)
			ham.preTrainCalibrate(train)
			ham.train(train)
			ham.postTrainCalibrate(dev, loss)
			ham.writeoutParameters(f)
		}
		//new NoTrainAligner(new LemmaMatch)
		ham
	}

	def getCPIE: CPInferenceEngine = {
		val ie = new CPInferenceEngine
		ie.setQuadraticFertilityCosts
		ie.enablePSAFactors
		ie.tempOrdMethod = TimeSieveTemporalFactors
		val f1 = new File("/home/hltcoe/twolfe/fall2013/parma/diagnostics/acl2014/1389289375/")
		val f2 = new File(f1, "Ablation-model=everything_host=r4n15.local_data=EECB_sgeTask=9_fert2Mode=max")
		val f3 = new File(f2, "parameters-corpusId=EECB-CV_wFeatures-fold0.txt")
		ie.readParameters(f3)
		ie
	}

	def getDAs: Seq[DocAlignment] = ConcreteDocAlignmentReader.EECB.getDocAlignments

	def main(args: Array[String]) {
	
		ParmaConfig.load("parma.config")
		teeLogTo(StdOutLogger, new FileLogger("diagnostics/dadiffer.txt"))

		val ham = getHAM
		val cpie = getCPIE
		val das = getDAs

		ham.preTrainCalibrate(das)
		cpie.preTrainCalibrate(das)

		val reports = das.map(_.report)
		val passages = das.map(_.passage)
		val domains = das.map(_.domain)

		val hypHAM = ham.align(reports, passages, domains)
		val hypCPIE = cpie.align(reports, passages, domains)
		val diffs = das.zip(hypHAM).zip(hypCPIE).map(x => {
			val ((d, hh), hc) = x
			DADiff(d, hh, hc)
		}).sortBy(-_.jointImprovement(loss))

		for(dad <- diffs) {
			val cpp = dad.cherryPickedPreds
			if(cpp.size > 0) {
				log(dad.toString)
				for(pa <- cpp) {

					log(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
					log("gold=1 CP=1 HAM=0")
					log(Describe.alignment(pa, dad.report, dad.passage, contextWordsEachSide=20) + "\n")

					// - other positives alignments from HAM in this row/col
					log("did ham align other stuff in a row or col?")
					var b = false
					for(hpp <- alsoAlignedInRow(pa.reportPred, dad.hypHAM, true)) {
						b = true
						log("row ham=1: " + Describe.alignment(hpp, dad.report, dad.passage))
					}
					for(hpp <- alsoAlignedInRow(pa.passagePred, dad.hypHAM, false)) {
						b = true
						log("col ham=1: " + Describe.alignment(hpp, dad.report, dad.passage))
					}
					if(!b) log("NO")

					// - the argument strcutre, and what is aligned
					// for(arg <- argsByCPG) print(g, cp, ham)
					val cpg = new CPGlobalAlignment(1)
					val ff = (a: Alignment, c: Context) => new SVec
					cpg.initData(ff, dad.gold, 1d)
					for(aa <- cpg.argAlignmentsFor(pa)) {
						val a: ArgCorefAlignment = aa.alignment
						val gL = dad.gold.possibleAlignments.contains(a)
						val hL = dad.hypHAM.possibleAlignments.contains(a)
						val cL = dad.hypCPIE.possibleAlignments.contains(a)
						log("gold=%s ham=%s cpie=%s".format(gL, hL, cL))
						log(Describe.alignment(a, dad.report, dad.passage))
						log("#######")
					}

					log("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
				}
				log("report: " + Describe.document(dad.report))
				log("passage: " + Describe.document(dad.passage))
				log("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
				log("goldDA: " + Describe.docAlignment(dad.gold))
				log("hamDA: " + Describe.docAlignment(dad.hypHAM))
				log("cpieDA: " + Describe.docAlignment(dad.hypCPIE))
				log("================================================================")
			}
		}
	}

	def alsoAlignedInRow(p: Predicate, da: DocAlignment, matchReport: Boolean): Seq[PredicateAlignment] = {
		val buf = new ArrayBuffer[PredicateAlignment]
		for(a <- DocMetaAligner.allPossiblePredAlignments(da.context)) {
			val matches = (matchReport && a.reportPred == p) || (!matchReport && a.passagePred == p)
			if(matches && da.possibleAlignments.contains(a))
				buf += a
		}
		buf.toSeq
	}


}

case class DADiff(val gold: DocAlignment, val hypHAM: DocAlignment, val hypCPIE: DocAlignment) {

	def report = gold.report
	def passage = gold.passage
	def context = gold.context

	lazy val hamInst = Seq(Instance(hypHAM, gold))
	lazy val cpieInst = Seq(Instance(hypCPIE, gold))
	def jointImprovement(loss: EvaluationLoss): Double =
		loss(hamInst) - loss(cpieInst)

	/**
	 * gold = 1
	 * cp = 1
	 * ham = 0
	 */
	def cherryPickedPreds: Seq[PredicateAlignment] = {
		val buf = new ArrayBuffer[PredicateAlignment]
		for(pa <- DocMetaAligner.allPossiblePredAlignments(context)) {
			val g = gold.possibleAlignments.contains(pa)
			val cp = hypCPIE.possibleAlignments.contains(pa)
			val ham = hypHAM.possibleAlignments.contains(pa)
			if(g && cp && !ham) buf += pa
		}
		buf.toSeq
	}

	override def toString: String = "(DADiff %s)".format(gold.id)
}






