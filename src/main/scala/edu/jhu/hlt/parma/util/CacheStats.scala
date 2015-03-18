package edu.jhu.hlt.parma.util

class CacheStats(val name: Option[String] = None, interval: Int = 10) extends Logging2 {
	def this(n: String) = this(Some(n))
	private var hits, misses = 0
	def hit {
		if(size % interval == 0) log(toString)
		hits = hits + 1
	}
	def miss {
		if(size % interval == 0) log(toString)
		misses = misses + 1
	}
	def size = hits + misses
	def clear {
		hits = 0
		misses = 0
	}
	def hitRate: Double = {
		if(size == 0) 0d
		else hits / size
	}
	def missRate: Double = {
		if(size == 0) 1d
		else misses / size
	}
	override def toString = "(%scaching: %d / %d hits)"
		.format(name match {
			case Some(n) => n + " "
			case None => ""
		}, hits, size)
}

