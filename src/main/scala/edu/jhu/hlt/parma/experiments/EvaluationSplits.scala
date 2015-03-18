// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.experiments

import scala.collection.mutable.ArrayBuffer

object EvaluationSplits {
	
	type SplitFunc[T] = Corpus[T] => Seq[Corpus[T]]
	
	/**
	 * folds the data in test and adds it to any prexisting data in train/dev
	 */
	def crossValidation[T](k: Int, addToDev: Boolean = false): SplitFunc[T] = {
		(c: Corpus[T]) => {
			val corps = new ArrayBuffer[Corpus[T]]
			for(i <- 0 until k) {	// test fold
				val j = (i+1) % k	// dev fold
				val te = new ArrayBuffer[T]
				val de = new ArrayBuffer[T]
				val tr = new ArrayBuffer[T]
				for((da, idx) <- c.test.zipWithIndex) {
					if(idx % k == i)
						te += da
					else {
						if(addToDev && idx % k == j)
							de += da
						else
							tr += da
					}
				}
				corps += new Corpus(c.id+"-fold"+i, c.train ++ tr, c.dev ++ de, te)
			}
			corps
		}
	}

	/**
	 * regular crossValidation will split test up into parts to add to train and test
	 * this method leaves what is in test there, and splits the training and dev data
	 * into folds which are used in train/dev
	 */
	def crossValidationLeaveTestAlone[T](k: Int): SplitFunc[T] = {
		(c: Corpus[T]) => {
			val corps = new ArrayBuffer[Corpus[T]]
			val drawFrom = c.train ++ c.dev
			for(i <- 0 until k) {
				val de = drawFrom.zipWithIndex.filter(_._2 % k == i).map(_._1)
				val tr = drawFrom.zipWithIndex.filter(_._2 % k != i).map(_._1)
				corps += new Corpus(c.id+"-fold"+i, tr, de, c.test)
			}
			corps
		}
	}
	
	/**
	 * dev will be kept in dev
	 * CV will occur on test items, anything in train
	 * will stay in train
	 */
	def leaveOneOut[T](k: Int): SplitFunc[T] = {
		(c: Corpus[T]) => {
			for((t, idx) <- c.test.zipWithIndex)
				yield new Corpus(c.id + ".loo" + idx, c.train ++ c.test.filter(_ != t), c.dev, Seq(t))
		}
	}
	
	def asIs[T] = (tdt: Corpus[T]) => Seq(tdt)

}

