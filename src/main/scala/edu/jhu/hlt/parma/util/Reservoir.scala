package edu.jhu.hlt.parma.util

import collection.mutable.ArrayBuffer
import util.Random

object Reservoir {
	implicit class Sample[T](val all: TraversableOnce[T]) {
		def reservoir(howMany: Int)(implicit rand: Random): IndexedSeq[T] = {
			val res = new ArrayBuffer[T]
			var n = 0
			all.foreach(item => {
				if(res.size < howMany)
					res += item
				else {
					val i = rand.nextInt(n)
					if(i < howMany)
						res(i) = item
				}
				n += 1
			})
			res.toIndexedSeq
		}
	}
}

