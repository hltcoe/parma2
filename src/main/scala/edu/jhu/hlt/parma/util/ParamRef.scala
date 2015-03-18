package edu.jhu.hlt.parma.util

import edu.jhu.hlt.parma.types.DVec

// use sparingly
trait ParamRef {
	def name: String
	def index: Int
	def value: Double
	override def toString: String = "(ParamRef %s @ %d = %.2g)".format(name, index, value)
}

case class FixedParamRef(override val name: String, override val index: Int, private val vec: DVec) extends ParamRef {
	def this(name: String, lookupIndexIn: Alphabet[String], vec: DVec) =
		this(name, lookupIndexIn.lookupIndex("fixed-param-ref:" + name, addIfNotPresent=true), vec)
	override def value: Double = vec(index)
}

