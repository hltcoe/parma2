// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma

import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.input._
import edu.jhu.hlt.parma.experiments._
import edu.jhu.hlt.parma.inference._
import collection.mutable.ArrayBuffer
import java.io.File
import java.util.Arrays

// DocAlignment must be given as (Discourse, Communication, Communication)+ file
	// could use (Communications, Alignments, Mentions) instead of (Discourse, Communication, Communication)+
// Doc pairs with no labels may be given as (Communication)+ file and a TSV of doc id pairs

object CLI {

	object Train extends Logging2 {
		def main(args: Array[String]) {
			val train = new ArrayBuffer[File]
			val tune = new ArrayBuffer[File]
			val test = new ArrayBuffer[File]
			var configFile: String = null
			var modelFile: File = null
			var addingTo: ArrayBuffer[File] = null
			var i = 0 
			while(i < args.length) {
				args(i) match {
					case "--train" =>
						addingTo = train
						i += 1
					case "--tune" =>
						addingTo = tune
						i += 1
					case "--test" =>
						addingTo = test
						i += 1
					case "--config" =>
						configFile = args(i+1)
						addingTo = null
						i += 2
					case "--model" =>
						modelFile = new File(args(i+1))
						addingTo = null
						i += 2
					case filename =>
						assert(addingTo != null, "args = " + args.toSeq)
						addingTo += new File(filename)
						i += 1
				}   
			}
			if(configFile == null)
				throw new IllegalArgumentException("you must provide a config file with --config")
			else ParmaConfig.load(configFile)
			if(modelFile == null)
				throw new IllegalArgumentException("you must provide a model file with --model")
			if(train.size == 0)
				throw new IllegalArgumentException("you must provide at least one train file with --train")
			
			type F = HAMFeatureRepresentation
			val pipe = new Pipeline[F]
			val model = pipe.run(new Experiment[InferenceEngine[F]] {	// TODO fix these broken generics
				override def rawData: DocAlignmentCorpus[DocAlignment] = {
					new DocAlignmentCorpus("cli-experiment",
						train.flatMap(ConcreteDocAlignmentUtils.deserialize),
						tune.flatMap(ConcreteDocAlignmentUtils.deserialize),
						test.flatMap(ConcreteDocAlignmentUtils.deserialize))
				}
				override def inferenceEngine: InferenceEngine[F] = new HierarchicalAlignmentModule
			})

			import java.io._
			log("writing model to " + modelFile.getPath)
			val oos = new ObjectOutputStream(new FileOutputStream(modelFile))
			oos.writeObject(model)
			oos.close
		}
	}

	object Predict {
		// Communications
		// TSV of doc id pairs
		// mention file
		// model file (to read from)
		// config file (options that we might need to fall back on)
		def main(args: Array[String]) {
			HierarchicalAlignmentModule.main(args)
		}
	}

	object Help {
		def main(args: Array[String]) {
			// TODO
			println("no help here")
		}
	}

	def main(args: Array[String]) {
		if(args.length == 0) {
			println("please provide a task: (train|predict|help)")
			return
		}
		val command = args(0)
		val argp = Arrays.copyOfRange(args, 1, args.length)
		command match {
			case "train" => Train.main(argp)
			case "predict" => Predict.main(argp)
			case "help" => Help.main(argp)
		}
	}
}

