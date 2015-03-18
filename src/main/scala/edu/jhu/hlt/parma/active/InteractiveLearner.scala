// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.active

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import java.io.File
import collection.mutable.ArrayBuffer

class InteractiveLearner {

	// label = sure|possible|no
	class Instance(val alignment: Alignment, val context: Context, val label: Option[String])

	// what is a feature function? (this will change)
	type Feature = (Alignment, Context) => Double

	var features = new ArrayBuffer[Feature]
	var labledExamples = new ArrayBuffer[Instance]
	var unlabledExamples = new ArrayBuffer[Instance]

	def workingSet {
		println("working with %d features, %d labled examples, and %d unlabled examples"
			.format(features.size, labledExamples.size, unlabledExamples.size))
		println(Describe.memoryUsage())
	}

	def help {
		println("TODO: this should print out what you can do with Machine")
	}

	def trainPerformance: Double = {
		throw new RuntimeException("implement me")
	}

	def testPerformance: Double = {
		throw new RuntimeException("implement me")
	}

	def loadExamples(discourseFile: File, communicationFile: File) {
		// TODO compute features
		throw new RuntimeException("implement me")
	}

	def addNewFeature(f: Feature) {
		features += f
		// TODO compute this feature for all examples
		// TODO retrain the model with this feature added
		throw new RuntimeException("implement me")
	}

	def chooseExampleHeuristically(label: Option[String] = None): Instance = {
		// TODO put your heuristic to "explore the feature space" here
		throw new RuntimeException("implement me")
	}

	def chooseExampleRandomly(label: Option[String] = None): Instance = {
		// TODO scan through unannotated examples and return one at random
		throw new RuntimeException("implement me")
	}

	def annotate(howMany: Int = 1) {
		for(i <- 1 to howMany)
			annotate(chooseExampleRandomly())
	}

	def annotate(inst: Instance) {
		// display the alignment to the user
		println(Describe.alignment(inst.alignment, inst.context.report, inst.context.passage))
		println("Is this aligned? [sure|possible|no]")

		// read back the users' annotation
		val response = System.console.readLine

		// TODO write out the annotation to a text file or database
		throw new RuntimeException("implement me")
	}

	def findExamplesMaximizing(score: Instance => Double, howMany: Int = 1): Seq[Instance] = {
		throw new RuntimeException("implement me")
	}

	def findExamplesWhere(predicate: Instance => Boolean): Seq[Instance] = {
		throw new RuntimeException("implement me")
	}
}


