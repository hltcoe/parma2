// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.experiments

import edu.jhu.hlt.parma.annotation.MTurkUtils
import edu.jhu.hlt.parma.math.{Stats, ConfidenceInterval}
import edu.jhu.hlt.parma.inference._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.diagnostics._
import edu.jhu.hlt.parma.evaluation._
import collection.mutable.HashMap
import collection.mutable.ArrayBuffer
import java.io._

object PipelineRunner {
	// for some reason this doesn't appear to work because of
	// how i'm calling java/scala from a shell script
	// (leaving this out because I already call Profiler.writeoutTimes below)
	/*
	Runtime.getRuntime.addShutdownHook(new Thread {
		override def run {
			println("inside shutdown hook")
			Profiler.writeoutTimes
		}
	})
	*/

    util.Random.setSeed(RandomUtils.randomSeed)
    // can't do this for java Random...

	def main(args: Array[String]) {
		val pipeline = new Pipeline[FeatureRepresentation]
		pipeline.main(args)
	}
}

/**
 * runs experiments specified in parma.config
 */
class Pipeline[F <: FeatureRepresentation] extends Logging2 {
	
	val modelSerializeKey = "diagnostics.serialize.model"
	
	def getExperiments: Seq[Experiment[InferenceEngine[F]]] = {
		ParmaConfig.getStrings(ParmaConfig.EXPERIMENTS).map(expName => {
			val fullName =
				if(expName.startsWith("edu.jhu.hlt.parma")) expName
				else "edu.jhu.hlt.parma.experiments." + expName
			try {
				Class.forName(fullName).newInstance.asInstanceOf[Experiment[InferenceEngine[F]]]
			} catch {
				case cnf: ClassNotFoundException =>
					throw new RuntimeException("could not find experiment named: " + fullName)
			}
		})
	}

	def main(args: Array[String]) {
		var success = false
		if(args.length == 1) {
			log("loading config file: " + args(0))
			ParmaConfig.load(args(0))
		}
		Profiler.startTask("total")
		GeneralDiagnostics.checkConfig
		try {
			for(experiment <- getExperiments)
				run(experiment)
			success = true
		} catch {
			case e: Exception => {
				log("grep for this for exceptions")
				e.printStackTrace
			}
		}
		Profiler.endTask("total")
		Profiler.writeoutTimes
		System.exit(if(success) 0 else -1)
	}
	
	def run(experiment: Experiment[InferenceEngine[F]]): InferenceEngine[F] = {

		val engine = experiment.inferenceEngine
		//val engine = loadInferenceEngine
		
		import DocAlignmentCorpusImplicits._
		var data: Corpus[DocAlignment] = Profiler.time("loadData", Unit => experiment.rawData)
		GeneralDiagnostics.checkCorpus(data)
		log(GeneralDiagnostics.corpusStatistics(data))
		
		// calibrate features / whatever else
		Profiler.startTask("preTrainCalibrate")
		log("[Pipeline run] calling preTrainCalibrate on " + engine.name)
		engine.preTrainCalibrate(data.all)
		Profiler.endTask("preTrainCalibrate")
		
		// TODO check if train and test alignments all have the same domain
		// if so, then we want to make sure that we don't use domain adaptation
		// so that we don't double our feature vector size and slow things down
		val domains = data.allAlignments.groupBy(_.domain)
		for((dom, as) <- domains)
			log("[Pipeline] domain %s has %d alignments ".format(dom, as.size))
		warnIf(domains.size > 1, "you have more than one domain but the domain adaptation code has been removed/broken")
		
		// compute features and keep them for the rest of the experiment
		Profiler.startTask("computeFeatures")
		log("about to compute features on all the data (this may take a while)...")
		val featureDumpDir = ParmaConfig.getDirectory("diagnostics.features.serialize", null)
		var pCount = 0
		def promote(da: DocAlignment) = {
			//Profiler.startTask("computeFeaturesForOneAlignment")
			val start = System.currentTimeMillis
			val dawf = engine.computeFeatures(da)
			//val t = Profiler.endTask("computeFeaturesForOneAlignment") / 1000d
			val t = (System.currentTimeMillis - start)/1000d
			val pa = DocMetaAligner.numPossibleAlignments(da.context)
			log("[Pipeline computeFeatures] %s alignment %d / %d size=%d took %.1f sec"
				.format(Describe.memoryUsage(timestamp=true), pCount, data.totalSize, pa, t))
			pCount += 1
			// TODO retur to this, need to serialize a whole crapload of stuff in a dawf
			// maybe force feature representations or inference engines to implement (de)serialize
			//if(featureDumpDir != null) {
			//	val f = new File(featureDumpDir, da.id + ".fv.gz")
			//	FeatureVectorIO.toFile(fv, f)
			//}
			dawf
		}
		val featurizedData = data.map(promote, "_wFeatures", verbose=true)
		val featureComputeTime = Profiler.endTask("computeFeatures") / 1000d
		log("done! took %.1f seconds".format(featureComputeTime))
		//data = null; System.gc
		

		// slice and dice the data however you want (given that the features are already computed)
		val losses = new ArrayBuffer[Double]
		val results = new HashMap[String, Seq[Double]]
		CosineVsF1.open
		implicit def fda2da(fda: DocAlignmentWithFeatures[F]): DocAlignment = fda.alignment
		for((split, splitNum) <- experiment.evaluationSplits(featurizedData).zipWithIndex) {
			log("#train = " + split.train.size)
			log("#dev = " + split.dev.size)
			log("#test = " + split.test.size)
			val (l, r) = runOnCorpus(engine, experiment, split)
			losses += l
			for((k,v) <- r)
				results(k) = results.getOrElse(k, Seq()) :+ v
		}
		CosineVsF1.close


		log("[Pipeline runExperiment] results=" + results)

		log("average loss on %s = %s".format(experiment.name, new ConfidenceInterval(losses).toString))
		for((k,vs) <- results) {
		//for(func <- experiment.evaluationFunctions) {
			//val vs = results(func.name)
			val ci = new ConfidenceInterval(vs)
			//log("average on %s, %s = %s".format(experiment.name, func.name, ci))
			log("average on %s, %s = %s".format(experiment.name, k, ci))
		}

		saveInferenceEngine(engine)
		engine
	}


	/**
	 * @deprecated make InferenceEngines do this themselves with writeoutParameters
	 */
	def saveInferenceEngine(engine: InferenceEngine[F]) {
		val f = ParmaConfig.getFile(modelSerializeKey)
		if(f != null && f.exists) {
			try {
				log("saving model to " + f.getPath)
				val oos = new ObjectOutputStream(new FileOutputStream(f))
				oos.writeObject(engine)
				oos.close
			}
			catch {
				case e: java.io.NotSerializableException =>
					e.printStackTrace
					warn("you asked to save %s to %s, but %s is not serializable!"
						.format(engine.name, f.getPath, engine.getClass.getName))
			}
		}
		else log("not saving model because %s was not set in parma.config".format(modelSerializeKey))
	}

	/**
	 * @deprecated make InferenceEngines do this themselves with readParameters
	 */
	def loadInferenceEngine: InferenceEngine[_] = {
		val f = ParmaConfig.getFile(modelSerializeKey)
		if(f != null && f.exists) {
			log("loading model from " + f.getPath)
			val ois = new ObjectInputStream(new FileInputStream(f))
			val model = ois.readObject.asInstanceOf[InferenceEngine[_]]
			ois.close
			model
		}
		else throw new RuntimeException("cannot find file: " + modelSerializeKey)
	}

	
	/**
	 * returns loss given by experiment.loss
	 * along with a hashmap containing other performance numbers
	 * the keys will have the form "(train|dev|test)-(feature.name)"
	 */
	def runOnCorpus(engine: InferenceEngine[F],
			experiment: Experiment[InferenceEngine[F]],
			corpus: Corpus[DocAlignmentWithFeatures[F]]): (Double, HashMap[String, Double]) = {
		
		// train (and maybe tune on dev)
		log("runOnCorpus about to train on %d examples...".format(corpus.train.size))
		Profiler.time("train", Unit => engine.train(corpus.train))
		if(corpus.dev.size > 0) {
			log("runOnCorpus about to devTune on %d examples...".format(corpus.dev.size))
			Profiler.time("devTune", Unit => engine.postTrainCalibrate(corpus.dev, experiment.loss))
		}

		// produce alignments on test data and evaluate
		Profiler.startTask("evaluation")
		val ret = if(corpus.test.size == 0) {
			warn("no test examples were give, nothing to evaluate on")
			(0d, new HashMap[String, Double])
		}
		else {
			log("runOnCorpus about to evaluate")
			
			var testLoss = -1d
			val results = new HashMap[String, Double]	// run evaluation functions specified by experiment
			Profiler.startTask("Pipeline:prediction")
			for((ds, dsName) <- Seq((corpus.test, "test"), (corpus.dev, "dev"), (corpus.train, "train")) if ds.size > 0) {

				Profiler.startTask("Pipeline:prediction:" + dsName)
				val predictions: Seq[DocAlignment] =
					engine.align(ds.map(_.features))
				val instances: Seq[Instance[DocAlignment]] =
					ds.map(_.alignment).zip(predictions).map(gold_hyp =>
						new Instance(gold_hyp._2, gold_hyp._1))
				EvaluationUtil.checkInstances(instances)
				Profiler.endTask("Pipeline:prediction:" + dsName)
				

				// save instances scores for bootstrap CI
				ScoreLogger.logScores(experiment.workingDirectory, dsName, instances)

				//for((dawf, inst) <- ds.zip(instances))
				//	FeatureDiagnostics.writeoutAlignmentFeatures(inst, dawf.features)
				//MTurkUtils.dumpAlignments(predictions, corpus.id + "-" + dsName)
				//CosineVsF1.analyze(instances, corpus.id + "-" + dsName)
				//CosineBySentenceVsF1.analyze(instances, corpus.id + "-" + dsName)
				
				Profiler.startTask("Pipeline:evaluationFunctions")
				for(func <- experiment.evaluationFunctions) {
					val score = func(instances)
					results.put(dsName + "." + func.name, score)
					log("experiment=%s, corpus=%s.%s, %s=%.3f".format(experiment.name, corpus.id, dsName, func.name, score))
				}
				Profiler.endTask("Pipeline:evaluationFunctions")
				
				val loss = experiment.loss(instances)
				log("experiment=%s, corpus=%s.%s, loss=%.3f".format(experiment.name, corpus.id, dsName, loss))

				if(dsName == "test") {
					testLoss = loss
					GeneralDiagnostics.outputPredictions(instances)
					experiment.workingDirectory match {
						case Some(wd) =>
							val f = wd.uniqFile(flags=Seq("parameters"), props=Map("corpusId"->corpus.id))
							engine.writeoutParameters(f)
						case None =>
							log("not writing out all the parameters because experiment.workingDirectory == None")
					}
					engine.logBiggestParameters(100)
				}

			}
			Profiler.endTask("Pipeline:prediction")
			log("[Pipeline runOnCorpus] results=" + results)
			(testLoss, results)
		}
		Profiler.endTask("evaluation")
		Profiler.writeoutTimes(getLogger.getOutputStream)
		ret
	}
	
}


