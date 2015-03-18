package edu.jhu.hlt.parma.inference.maxmargin

import edu.jhu.hlt.parma.inference.DocMetaAligner
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.input._
import util.Random
import java.io.File

/**
 * this is totally useless now
 * I was on a witch-hunt to figure out why my constraints were growing in size,
 * and I think I've just discovered it was because I was multiplying in a weight
 * and then dividing it out again.
 * I'm going to comment this out, and if it works, I shouldn't need this code.
 */
object ScalingTests extends Logging2 {

/*
	implicit val rand = new Random(9001)
	ParmaConfig.load("parma.config")
	teeLogTo(StdOutLogger, new FileLogger("diagnostics/scaling-tests.log"))

	val ie = new CPInferenceEngineFast
	ie.redirectLogTo(this)
	lazy val das: Seq[DocAlignment] = ConcreteDocAlignmentReader.EECB.getDocAlignments
		.filter(da => DocMetaAligner.allPossibleAlignments(da.context).size < 500)
	lazy val dawfs: Seq[DocAlignmentWithFeatures[CPFeatureRepr]] = das.map(ie.computeFeatures)
	lazy val cpas: Seq[CPGlobalAlignment] = dawfs.map(_.features.cpAlignment)

	def main(args: Array[String]) {
		
		if(args.size > 0 && args(0).equalsIgnoreCase("shuf")) {
			shuffleEECB()
			return
		}

		ie.verbose = false
		val ptt = Profiler.getTime {
			ie.preTrainCalibrate(das)
		}
		log("pretraining too %.1f seconds".format(ptt))

		log("\n*************** SUPER BASICS ***************")
		superBasics

		log("\n*************** SIMPLE ***************")
		simple

		log("\n*************** MARGIN ***************")
		sizeOfMarginShouldBeInvariantToAmountOfData
	}
*/

	/**
	 * it appears that the doc alignments in the EECB corpus are not very homogenous
	 * in terms of their size. this will shuffle them in a new file
	def shuffleEECB(outfile: File = new File("data/eecb/eecb-alignments-shuf.concrete.pb")) {
		require(!outfile.exists, "i wont overwrite unless you tell me to")
		val t = Profiler.getTime {
			val das = ConcreteDocAlignmentReader.EECB.getDocAlignments.toBuffer
			val shufdas = rand.shuffle(das)
			ConcreteDocAlignmentUtils.serialize(shufdas, outfile)
		}
		log("shuffled EECB to %s in %.1f seconds".format(outfile.getPath, t))
	}
	 */

	/**
	 * need to figure out why avgDF.l1/avgLoss != df.l1/loss
	 * 
	 * avgDF = 1/N \sum_i df_i
	 * avgLoss = 1/N \sum_i loss_i
	 *
	 * df = ???
	def superBasics {

		val k = 30
		System.setProperty("inference.ssvm.macro-constraint-weighting", "999999")
		ie.setAvgAlignmentSize(cpas.take(k))
		val constraints = cpas.take(k).map(cpa => {
			val cons = new Constraint(ie.theta.dimension)
			cpa.addMostViolatedConstraint(ie.theta, cons)
			cons
		})

		val norm = (vec: DVec) => vec.l2


		// SUPER MEGA BASIC
		val vec: DVec = constraints.head.debugGetRawValues._1
		log("vec =" + vec)
		log("norm(vec) = " + norm(vec))
		val vecScaled = vec.copy
		vecScaled *= (1/2d)
		log("vecScaled =" + vecScaled)
		log("norm(vecScaled) = " + norm(vecScaled))


		var sumLoss = 0d
		var sumN = 0d
		val sumDF = DVec.zero(ie.theta.dimension)
		val sumCons = new Constraint(ie.theta.dimension)
		for(c <- constraints) {
			val (df, loss, n) = c.debugGetRawValues
			sumCons += c
			sumLoss += loss
			sumN += n
			sumDF += df

			val avgDF = sumDF * (1d/sumN)
			val avgLoss = sumLoss / sumN

			//log("norm(avgDF)=%f norm(cons.df)=%f cons.getLoss=%f avgLoss=%f"
			//	.format(norm(avgDF), norm(sumCons.getDeltaFeatures), sumCons.getLoss, avgLoss))

			log(diffStr("sumN", sumN, sumCons.getN))
			log(diffStr("norm(df)", norm(avgDF), norm(sumCons.getDeltaFeatures)))

			log("norm(sumCons.deltaFeatures)=%.2f norm(sumCons.getDeltaFeatures)=%.2f"
				.format(norm(sumCons.debugGetRawValues._1), norm(sumCons.getDeltaFeatures)))
		}

	}
	 */

	def diffStr(name: String, actual: Double, estimate: Double): String =
		"%s diff=%.2f%% actual=%.1f estimate=%.1f".format(name, percentDiff(actual, estimate), actual, estimate)

	def percentDiff(actual: Double, estimate: Double): Double =
		100d * (estimate - actual) / actual

	/**
	 * do the addMostViolatedConstraint loop from CPInferenceEngine:
	 * the ratio of df.l2 to loss should converge
	def simple {

		System.setProperty("inference.ssvm.macro-constraint-weighting", "999999")
		ie.setAvgAlignmentSize(cpas)

		// maybe DF is growing disproportionately because i'm not accounting for the number
		// of factors, only the number of alignment varaibles
		// => if i disable all factors other than CPAlignments, everything should look normal
		// ===> NOPE. nothing changes when you do this
		//ie.disableSharedArgQuadraticCosts
		//ie.disableFertilityCosts
		//ie.tempOrdMethod = NoTemporalFactors

		val sumDF = DVec.zero(ie.theta.dimension)
		var sumLoss = 0d
		var sumN = 0d
		val cons = new Constraint(ie.theta.dimension)
		for((c, i) <- cpas.take(300).zipWithIndex) {
			val oneCons = new Constraint(ie.theta.dimension)
			c.addMostViolatedConstraint(ie.theta, oneCons)
			cons += oneCons
			val df = cons.getDeltaFeatures
			val dfl2 = df.l2
			val dfl1 = df.l1
			val loss = cons.getLoss
			require(loss >= 0, "loss = " + loss)
			val n = cons.getN
			log("[simple] i=%d n=%.1f df.l2=%.1f df.l1=%.1f loss=%.1f df.l2/loss=%.1f df.l1/loss=%.1f loss/n=%.3f df.l2/n=%.1f df.l1/n=%.1f cpa.size=%d cpa.weight=%.2f"
				.format(i, n, dfl2, dfl1, loss, dfl2/loss, dfl1/loss, loss/n, dfl2/n, dfl1/n, c.size, c.constraintWeight))


			val (rawDF, rawLoss, rawN) = oneCons.debugGetRawValues
			require(rawLoss >= 0d, "rawLoss = " + rawLoss)
			sumDF += rawDF
			sumLoss += rawLoss
			sumN += rawN
			log("[simple]   sumDF.l2=%.1f sumDF.l1=%.1f sumLoss=%.1f avgDF.l2=%.1f avgDF.l1=%.1f avgLoss=%.1f sumDF.l2/sumLoss=%.1f sumDF.l1/sumLoss=%.1f"
				.format(sumDF.l2, sumDF.l1, sumLoss, sumDF.l2/(i+1d), sumDF.l1/(i+1d), sumLoss/(i+1d), sumDF.l2/sumLoss, sumDF.l1/sumLoss))

			// hypothesis: dfl2/loss = avgDF.l2 / avgLoss
			val avgLoss = sumLoss / sumN
			val avgDF = sumDF * (1d/sumN)
			log("[simple]     avgDF.l2/avgLoss=%.1f avgDF.l1/avgLoss=%.1f".format(avgDF.l2/avgLoss, avgDF.l2/avgLoss))

			log("")
		}
	}
	 */
	
	/**
	 * the thing that I'm worried about is the Constraints being fed to cplex
	 * they are:
	 *   xi + dot(theta, deltaFeatures) \ge loss * lossMult
	 *
	 * since both deltaFeatures and loss should be averages
	 * (weighted either as macro or micro, depending on inference.ssvm.macro-constraint-weighting)
	 * they should scale without meaning that xi is on a different scale
	 * (i.e. requiring lossMult to change)
	 *
	 * it appears that the ratio of df.l2 / loss is growing with training set size
	 * this means that (all other things being equal), theta does not need to be as
	 * big to satisfy the constraints. this leads to needing to increase either
	 * lossMult or svmC.
	 *
	 * earlier I thought that I was normalizing only one of two times...
	 * 
	def sizeOfMarginShouldBeInvariantToAmountOfData {

		import edu.jhu.hlt.parma.util.Reservoir._

		val samples = 3

		for(lam <- Seq(0d, 10d, 100d, 1000d)) {

			// train for a few iters to make sure everything is setup
			log("\nlam=%f\tsetting constraint weights...".format(lam))
			System.setProperty("inference.ssvm.macro-constraint-weighting", lam.toString)
			val t = Profiler.getTime {
				ie.setAvgAlignmentSize(cpas)
			}
			log("setting avg alignment size took %.2f seconds".format(t))

			for(k <- Seq(5, 15, 45, 135, 400)) {

				val conssample: Seq[Constraint] = (1 to samples).map(x => {
					log("training on %d alignments".format(k))
					val examples = dawfs.reservoir(k)
					val (time, (cons, fromCache)) = Profiler.getTimeAndValue {
						ie.mostViolatedConstraint(examples, useCache=false)
					}
					require(!fromCache)
					cons
				})

				// sum up the sample (Constraint keeps track of N for me)
				val cons = conssample.reduce(_ + _)
				val h = cons.hinge(ie.theta)
				val df = cons.getDeltaFeatures.l2
				log("hinge[k=%d] = hinge=%f df/loss=%f hinge-by-size=%f df/loss/size=%f getDF.l2=%.2f getLoss=%.2f n=%.2f"
					.format(k,            h, df/cons.getLoss, h/k,    df/cons.getLoss/k,     df, cons.getLoss, cons.getN))
			}
			log("\n")
		}
	}
	 */
	
}

