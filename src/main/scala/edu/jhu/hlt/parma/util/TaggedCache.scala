package edu.jhu.hlt.parma.util

import collection.mutable.ArrayBuffer

/**
 * cache with "approximate set behavior" dictated by
 * a tag provided upon adding items. Elements should be
 * immutable (tag is never updated).
 *
 * Set behavior is that upon adding an element with tag=X,
 * if there are any other elements in the cache with tag=X,
 * the item will not be added (and nothing will be evicted).
 */
class TaggedCache[T](val maxSize: Int) extends Iterable[T] {

	private[this] val cache = new ArrayBuffer[T]
	private[this] val tags = Array.ofDim[Long](maxSize)
	private[this] var idx = 0	// current index of elem to be evicted

	def containsTag(tag: Long): Boolean =
		tags.iterator.take(cache.size).contains(tag)

	/**
	 * returns true if the set did not already contain this item (tag)
	 */
	def add(item: T, tag: Long): Boolean = {
		if(!containsTag(tag)) {
			if(cache.size < maxSize) {
				tags(cache.size) = tag
				cache += item
			} else {
				tags(idx) = tag
				cache(idx) = item
				idx += 1
				if(idx == maxSize) idx = 0
			}
			true
		}
		else false
	}

	override def iterator: Iterator[T] = cache.iterator

	override def size = cache.size

	def clear {
		cache.clear
		idx = 0
	}

}

object TaggedCacheTests {

	def main(args: Array[String]) {
	
		val c1 = new TaggedCache[String](3)
		val a1 = c1.add("first string", 1)
		require(a1)
		require(c1.iterator.size == 1)
		require(c1.containsTag(1))

		val a2 = c1.add("second string", 2)
		require(a2)
		require(c1.iterator.size == 2)
		require(c1.containsTag(1))
		require(c1.containsTag(2))

		val a3 = c1.add("first string collison", 1)
		require(!a3)
		require(c1.iterator.size == 2)
		require(c1.containsTag(1))
		require(c1.containsTag(2))

		val a4 = c1.add("third string", 3)
		require(a4)
		val a5 = c1.add("fourth string", 4)
		require(a5)
		require(c1.iterator.size == 3)	// maxSize=3
		require(c1.containsTag(4))
		require(c1.containsTag(2))
		require(c1.containsTag(3))


	}
}

