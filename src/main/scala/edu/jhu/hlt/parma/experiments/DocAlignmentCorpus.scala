// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.experiments

import edu.jhu.hlt.parma.types.DocAlignment
import edu.jhu.hlt.parma.util.RandomUtils
import util.Random

object DocAlignmentCorpusImplicits {
	implicit def p[T<:DocAlignment](c: Corpus[T]): DocAlignmentCorpus[T] =
		new DocAlignmentCorpus(c.id, c.train, c.dev, c.test)
}

class DocAlignmentCorpus[+T <: DocAlignment](id: String, train: Seq[T], dev: Seq[T], test: Seq[T])
		extends Corpus[T](id, train, dev, test) {
	
	def this(c: Corpus[T]) = this(c.id, c.train, c.dev, c.test)
  	
	def trainAlignments = train
	def devAlignments = dev
	def testAlignments = test

	def allAlignments = train ++ dev ++ test
	
	def trainDocs = train.flatMap(a => List(a.report, a.passage)).toSet
	def devDocs = dev.flatMap(a => List(a.report, a.passage)).toSet
	def testDocs = test.flatMap(a => List(a.report, a.passage)).toSet
}

object DocAlignmentCorpus {

	val rand = new util.Random(RandomUtils.randomSeed)
	
	def randomSplit[T](items: Seq[T], propFirstBucket: Double): (Seq[T], Seq[T]) = {
		assert(propFirstBucket < 1d)
		val a = (items.size * propFirstBucket + 0.5d).toInt
		val shuf = rand.shuffle(items)
		(shuf.take(a), shuf.drop(a))
	}

	def randomSplit[T <: DocAlignment](id: String, items: Seq[T], propFirstBucket: Double): DocAlignmentCorpus[T] = {
		val (train, test) = randomSplit(items, propFirstBucket)
		new DocAlignmentCorpus(id, train, Seq(), test)
	}

	def randomSplit[T](items: Seq[T], propFirstBucket: Double, propSecondBucket: Double): (Seq[T], Seq[T], Seq[T]) = {
		assert(propFirstBucket < 1d)
		assert(propSecondBucket < 1d)
		assert(propFirstBucket + propSecondBucket < 1d)
		val a = (items.size * propFirstBucket + 0.5d).toInt
		val b = (items.size * propSecondBucket + 0.5d).toInt
		val shuf = rand.shuffle(items)
		(shuf.take(a), shuf.drop(a).take(b), shuf.drop(a+b))
	}

	def randomSplit[T <: DocAlignment](id: String, items: Seq[T], propFirstBucket: Double, propSecondBucket: Double): DocAlignmentCorpus[T] = {
		val (train, dev, test) = randomSplit(items, propFirstBucket, propSecondBucket)
		new DocAlignmentCorpus(id, train, dev, test)
	}
}

