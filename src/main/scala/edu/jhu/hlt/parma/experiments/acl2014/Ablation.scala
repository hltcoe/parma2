package edu.jhu.hlt.parma.experiments.acl2014

import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.input._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.inference._
import edu.jhu.hlt.parma.inference.maxmargin._
import edu.jhu.hlt.parma.features.LemmaMatch
import edu.jhu.hlt.parma.evaluation._
import edu.jhu.hlt.parma.experiments._
import java.io.File
import collection.mutable.ArrayBuffer

object Ablation {
	def main(args: Array[String]) {
		println("[Ablation main] args=[\"%s\"]".format(args.mkString("\", \"")))
		val a = new Ablation
		if(args.length != 3) {
			println("please provide:")
			println("1) an ArrayJob number (1-indexed)")
			println("2) a parma.config file")
			println("3) a parent working directory for output")
			if(args.length == 0)
				println(a.getArrayJob.helpString())
			if(args.length == 1)
				println(a.getArrayJob.helpString(999))
		}
		else a.scalaShouldReallyUseSeparateClassNamesForCompanionObjects(args)
	}
}

class Ablation extends Logging2 {
	
	type FR <: FeatureRepresentation
	type IE = InferenceEngine[FR]

	val debug = false
	val hamDevProp = 0.2d

	def getArrayJob = {
		val ajh = new ArrayJobHelperWithConstraints

		if(debug) {
			ajh.addParam("fnSmooth", Seq(5))
			ajh.addParam("fnBias", Seq(9))
			ajh.addParam("fert2Mode", fert2Modes)
			ajh.addParam("svmC", Seq(15d))
			ajh.addParam("svmD", Seq(2d))
			ajh.addParam("data", Seq("EECB"))
			ajh.addParam("model", Seq("parma+fert0", "parma+fert2_mallet", "parma+TStime", "parma+psa", "parma"))
		}
		else {
			ajh.addParam("fnSmooth", Seq(5))
			ajh.addParam("fert2Mode", fert2Modes)
			ajh.addParam("fnBias", Seq(9, 4, 16, 1))
			//ajh.addParam("nTrain", Seq(40, 80, 160))
			ajh.addParam("nTrain", Seq(9999))
			ajh.addParam("svmC", Seq(0.3d, 3d, 30d))
			ajh.addParam("svmD", Seq(2d, 0.5d, 8d, 4d, 1d))
			ajh.addParam("data", datasets.keys.toSeq)
			//ajh.addParam("data", Seq("EECB"))
			ajh.addParam("model", models.keys.toSeq)
			//ajh.addParam("model", Seq("parma+fert0", "parma+fert2_mallet", "parma+psa", "parma"))
		}

		// return false if you want to prune this configuration
		ajh.addConstraint((params: Map[String, String]) => {
			val m = params("model")
			if(m == "parma") {
				params("svmC") == ajh.getPossibleValues("svmC").min &&
					params("svmD") == ajh.getPossibleValues("svmD").min
			}
			else true
		})

		/*
		ajh.addConstraint((params: Map[String, String]) => {
			val m = params("model")
			val needToSweepFert2Mode = (m.contains("parma2") || m.contains("fert2") || m == "everything")
			needToSweepFert2Mode || params("fert2Mode") == "max"
		})
		*/

		// don't sweep anything if its lemma
		/*ajh.addConstraint((params: Map[String, String]) => {
			val m = params("model")
			if(m.contains("lemma"))
				params("fert2Mode") == "sum" && params("
			else true
		})*/

		ajh
	}

	// formerly "main"
	def scalaShouldReallyUseSeparateClassNamesForCompanionObjects(args: Array[String]) {

		val ajh = getArrayJob
		val taskId = args(0).toInt
		if(taskId < 0) {
			println(ajh.helpString(show=999))
			return
		}

		ParmaConfig.load(args(1))
		val parentOfWorkingDir: File = new File(args(2))
		require(parentOfWorkingDir.isDirectory)

		val params: Map[String, String] = ajh.getParams(taskId)	//ajh.getParams(args).get

		val f = new WorkingDirectory(parentOfWorkingDir).uniqFile(flags=Seq("Ablation"), props=params, suffix="", overwrite=true)
		val workingDir = new WorkingDirectory(f)
		teeLogTo(StdOutLogger, new FileLogger(workingDir / "log.txt"))
		log("logging to %s".format(f.getPath))
		log("got here, params=%s".format(params))
		log("host=" + System.getenv("HOSTNAME"))
		log("sgeTask=" + args(0))

		System.setProperty("inference.ssvm.fert2-mode", params("fert2Mode"))
		System.setProperty("inference.ssvm.svm-c", params("svmC"))
		System.setProperty("inference.ssvm.loss-mult", params("svmD"))
		System.setProperty("inference.ssvm.false-neg-bias", params("fnBias"))
		System.setProperty("inference.ssvm.loss-fn-smooth", params("fnSmooth"))
		System.setProperty("inference.num-train-limit", params("nTrain"))
		log("using fert2 mode = " + ParmaConfig.getString("inference.ssvm.fert2-mode"))

		import scala.language.existentials
		lazy val (corpus, splitter) = datasets(params("data")).apply()
		val modelName: String = params("model")
		val ie: IE = models(modelName).apply()
		ie.redirectLogTo(this)

		// see if we already have a model trained
		checkForPretrainedModel(modelName, ie)

		// make an anonymous Experiment and pass it to a Pipeline
		val experiment = new Experiment[IE] {
			override def name: String =
				"Ablation_" + params.map(kv => kv._1 + "=" + kv._2).mkString("_")
			override def workingDirectory: Option[WorkingDirectory] = Some(workingDir)
			override def rawData: Corpus[DocAlignment] = corpus
			override def inferenceEngine: IE = ie
			override def evaluationSplits[T](c: Corpus[T])(implicit asDocAlignment: T => DocAlignment): Seq[Corpus[T]] = {
				// what is the type signature for a polymorphic function??
				val corps: Seq[Corpus[T]] = splitter.asInstanceOf[Corpus[T] => Seq[Corpus[T]]](c)

				// add dev data for parma (if needed)
				corps.map(c => {
					if(ie.isInstanceOf[HierarchicalAlignmentModule] && c.dev.size == 0) {
						val (dev, train) = DocAlignmentCorpus.randomSplit(c.train, hamDevProp)
						new Corpus(c.id, train, dev, c.test)
					}
					else c
				})
			}
		}
		val pipeline = new Pipeline[FR] {
			override def getExperiments: Seq[Experiment[IE]] = Seq(experiment)
		}
		pipeline.redirectLogTo(this)
		pipeline.main(Array())
		ie.flushLog
	}

	val fert2Modes = Seq("max")//, "sum")

	/**
	 * never return dev data
	 * for EECB, return 5-fold CV corpi
	 * everything else just do regular train/test
	 */
	val datasets: Map[String, Unit => (Corpus[DocAlignment], EvaluationSplits.SplitFunc[_])] = {

		val sets = new ArrayBuffer[(String, Unit => (Corpus[DocAlignment], EvaluationSplits.SplitFunc[_]))]

		sets += ("EECB", Unit => {
			val all: Seq[DocAlignment] = ConcreteDocAlignmentReader.EECB.getDocAlignments
			if(debug) {
				// 437 documents in test
				// keep the test set large-ish and fixed
				val (possibleTrain, test) = all.splitAt(300)
				val nTrain = 80	// can go up to 200 w/o clipping dev data
				val train = possibleTrain.take(nTrain)
				val dev = possibleTrain.drop(nTrain).take(nTrain / 2)
				val c = new Corpus[DocAlignment]("EECB-CV", train, dev, test)
				(c, EvaluationSplits.asIs)
			}
			else {
				val c = new Corpus[DocAlignment]("EECB-CV", Seq(), Seq(), all)
				(c, EvaluationSplits.crossValidation(5, addToDev=true))
			}
		})

		sets += ("RF", Unit => {

			val train = new RothFrankDocAlignmentReader(true, false).getDocAlignments
			val test = new RothFrankDocAlignmentReader(false, true).getDocAlignments

			/*
			// augment with some EECB data that is similar to the RF data
			val rfTrainStats = new DAStats(train)
			val eecbAugment = ConcreteDocAlignmentReader.EECB.getDocAlignments
				.sortBy(da => rfTrainStats.divergence(da))
				.take(8)
			*/

			val n = train.size / 2
			val c = new Corpus[DocAlignment]("RF", train.take(n), train.drop(n), test)
			(c, EvaluationSplits.crossValidation(10, addToDev=true))
		})

		sets.toMap
	}

	/**
	 * determines if we already have a model trained and sets ie
	 * to read from that location if available
	 */
	def checkForPretrainedModel(modelName: String, ie: IE) {
		if(ie.isInstanceOf[CPInferenceEngine]) {
			val cpie = ie.asInstanceOf[CPInferenceEngine]
			val key = "experiment.ssvm.preTrained." + modelName
			val fname = ParmaConfig.getString(key, "none")
			if(fname != "none") {
				log("[Ablation checkForPretrainedModel] setting CPInferenceEngine to read from " + fname)
				val f = new File(fname)
				require(f.isFile, f.getPath + " is not a file, so you cannot load it as params for " + modelName)
				cpie.readParamsInsteadOfTraining = Some(f)
			}
		}
	}

	val models: Map[String, Unit => IE] = {

		val models = new ArrayBuffer[(String, Unit => InferenceEngine[_])]

		// baselines
		models += ("parma", Unit => new HierarchicalAlignmentModule)
		models += (("lemma", Unit => new NoTrainAligner(new LemmaMatch)))

		// this should roughly tie parma
		models += ("parma+fert0", Unit => {
			val ie = new CPInferenceEngine
			ie.disableFertilityCosts
			ie.disablePSAFactors
			ie.disableASPFactors
			ie.tempOrdMethod = NoTemporalFactors
			ie
		})

		/* never works... infeasible QP constraint (?!)
		models += ("parma2_cplex", Unit => {
			val ie = new CPInferenceEngineFast
			ie.setQuadraticFertilityCosts
			ie.enablePSAFactors
			ie.disableASPFactors
			ie.tempOrdMethod = TimeSieveTemporalFactors
			ie
		})
		*/

		models += ("everything", Unit => {
			val ie = new CPInferenceEngine
			ie.setQuadraticFertilityCosts
			ie.enablePSAFactors
			ie.enableASPFactors
			ie.tempOrdMethod = TimeSieveTemporalFactors
			ie
		})

		models += ("parma+fert1", Unit => {
			val ie = new CPInferenceEngine
			ie.setFirstOrderFertilityCosts
			ie.disablePSAFactors
			ie.disableASPFactors
			ie.tempOrdMethod = NoTemporalFactors
			ie
		})
		
		/*
		models += ("parma+fert2_cplex", Unit => {
			val ie = new CPInferenceEngineFast
			ie.setQuadraticFertilityCosts
			ie.disablePSAFactors
			ie.disableASPFactors
			ie.tempOrdMethod = NoTemporalFactors
			ie
		})
		*/

		models += ("parma+fert2_mallet", Unit => {
			val ie = new CPInferenceEngine
			ie.setQuadraticFertilityCosts
			ie.disablePSAFactors
			ie.disableASPFactors
			ie.tempOrdMethod = NoTemporalFactors
			ie
		})

		models += ("parma+psa", Unit => {
			val ie = new CPInferenceEngine
			ie.disableFertilityCosts
			ie.enablePSAFactors
			ie.disableASPFactors
			ie.tempOrdMethod = NoTemporalFactors
			ie
		})
		
		models += ("parma+asp", Unit => {
			val ie = new CPInferenceEngine
			ie.disableFertilityCosts
			ie.disablePSAFactors
			ie.enableASPFactors
			ie.tempOrdMethod = NoTemporalFactors
			ie
		})
		
		models += ("parma+TStime", Unit => {
			val ie = new CPInferenceEngine
			ie.disableFertilityCosts
			ie.disablePSAFactors
			ie.disableASPFactors
			ie.tempOrdMethod = TimeSieveTemporalFactors
			ie
		})
		
		models += ("parma+faketime", Unit => {
			val ie = new CPInferenceEngine
			ie.disableFertilityCosts
			ie.disablePSAFactors
			ie.disableASPFactors
			ie.tempOrdMethod = FakeTemporalFactors(1d)	// ~73% pairwise accuracy (high)
			ie
		})

		models.toMap
			.asInstanceOf[Map[String, Unit => IE]]	// i give up with this crap
	}
}

