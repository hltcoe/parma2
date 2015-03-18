package edu.jhu.hlt.parma.util

import edu.jhu.hlt.parma.types.DVec

trait ParamRefRange {
	def index(localIdx: Int): Int
	def value(localIdx: Int): Double
	def size: Int

	/**
	 * takes the indices in features as local indices
	 * and calls value(localIdx) to do the dot product
	 */
	def dot(features: DVec): Double = {
		val n = size
		assert(n == features.dimension,
			"trying to take the dot product of %d and %d dimensional vectors".format(n, features.dimension))
		var s = 0d
		var i = 0
		while(i < n) {
			s += value(i) * features(i)
			i += 1
		}
		s
	}
}

class AlphParamRefRange(val alph: Alphabet[String], val names: IndexedSeq[String], val theta: DVec) extends ParamRefRange {
	val globalIdx: IndexedSeq[Int] = names.map(alph.lookupIndex(_, addIfNotPresent=true))
	override def index(localIdx: Int) = globalIdx(localIdx)
	override def value(localIdx: Int) = theta(index(localIdx))
	override def size: Int = names.size

	/**
	 * make a ParamRef for every item in this range
	 */
	def paramRefs: Seq[ParamRef] =
		for((name, idx) <- names.zip(globalIdx))
			yield FixedParamRef(name, idx, theta)
}

class FixedParamRefRange(val offset: Int, val names: IndexedSeq[String], val theta: DVec) extends ParamRefRange {
	override def index(localIdx: Int): Int = offset + localIdx
	override def value(localIdx: Int): Double = theta(offset + localIdx)
	override def size: Int = names.size

	/**
	 * make a ParamRef for every item in this range
	 */
	def paramRefs: Seq[ParamRef] =
		for((name, idx) <- names.zipWithIndex)
			yield FixedParamRef(name, offset+idx, theta)
}

object ParamRefRangeImplicits {
	implicit def paramRef2paramRefRange(pr: ParamRef with Active): ParamRefRange with Active = {
		new ParamRefRange with Active {
			override def index(localIdx: Int): Int =
				if(localIdx == 0) pr.index
				else throw new IllegalArgumentException
			override def value(localIdx: Int): Double =
				if(localIdx == 0) pr.value
				else throw new IllegalArgumentException
			override def size: Int = 1
			override def active: Boolean = pr.active
		}
	}
}

