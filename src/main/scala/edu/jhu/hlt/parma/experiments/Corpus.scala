// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.experiments

import edu.jhu.hlt.parma.input._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util.RandomUtils
import scala.util.Random

class Corpus[+T](val id: String, val train: Seq[T], val dev: Seq[T], val test: Seq[T]) {

	def all = train ++ dev ++ test
	def totalSize = train.size + dev.size + test.size

	//  def this(id: String, alignments: Traversable[T], numTrain: Int) =
	//  	this(id, alignments.take(numTrain).toBuffer, Seq(), alignments.drop(numTrain).toBuffer)
	//  	
	//  def this(id: String, alignments: Traversable[T], propTrain: Double) =
	//  	this(id, alignments, (alignments.size * propTrain).toInt)

	/**
	 * use this for normalization and feature caching
	 */
	def map[B](f: T => B, appendToId: String = "_mapped", verbose: Boolean = false) = {
		val step = 1
		var i = 0
		val start = System.currentTimeMillis
		val n = train.size + dev.size + test.size
		def update(t: T) = {
			if (verbose && i % step == 0) {
				//print("*")
				val taken = (System.currentTimeMillis - start) / 1000d
				val rate = i.toDouble / taken
				val remaining = (n - i) / rate
				println("[Corpus map] done with %d/%d, %.1f sec taken, %.1f sec (est) remaining".format(i, n, taken, remaining))
				i += 1
			}
			f(t)
		}

/*
		// TODO when you have more time, experiment with parallel
		// feature computation. i think the only hard part is dealing
		// with contension for shared data in the feature fucntions...
		import scala.collection.parallel._
		val nThreads = 4

		val trainPar = train.par
		trainPar.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(nThreads))
		println("trainPar.parallelismLevel = " + trainPar.tasksupport.parallelismLevel)
		val trainMapped = trainPar.map(update).seq

		val devPar = dev.par
		devPar.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(nThreads))
		println("devPar.parallelismLevel = " + devPar.tasksupport.parallelismLevel)
		val devMapped = devPar.map(update).seq

		val testPar = test.par
		testPar.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(nThreads))
		println("testPar.parallelismLevel = " + testPar.tasksupport.parallelismLevel)
		val testMapped = testPar.map(update).seq

		new Corpus[B](id + appendToId, trainMapped, devMapped, testMapped)
*/
		new Corpus[B](id + appendToId, train.map(update), dev.map(update), test.map(update))
	}

}
