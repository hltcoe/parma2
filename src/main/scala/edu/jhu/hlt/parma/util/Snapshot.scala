package edu.jhu.hlt.parma.util

object Snapshot {
	// syntactic sugar: `snapshot.delta` => `delta(snapshot)`
	def delta[T](snapshot: Snapshot[T])(implicit n: Numeric[T]): T = snapshot.delta(n)
}

case class Snapshot[T](val prev: T, val cur: T) {
	def delta(implicit n: Numeric[T]): T = n.minus(cur, prev)
}

