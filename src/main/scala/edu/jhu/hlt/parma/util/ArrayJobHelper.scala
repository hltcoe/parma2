package edu.jhu.hlt.parma.util

import collection.mutable.{HashMap, ArrayBuffer}

class ArrayJobHelperWithConstraints extends ArrayJobHelper {

	// this version explicitly rather than implicitly stores configurations
	private val ajh = new ArrayJobHelper
	private var configs: IndexedSeq[Map[String, String]] = null
	val constraints = new ArrayBuffer[Map[String, String] => Boolean]

	override def getPossibleValues(paramName: String): Seq[String] =
		ajh.getPossibleValues(paramName)

	/**
	 * earlier calls to this method result in less-embedded loops
	 * put another way, the last call to this will be the first
	 * range of values swept
	 */
	override def addParam[T](name: String, values: Seq[T]) {
		ajh.addParam(name, values)
		configs == null
	}
	
	/**
	 * only return a configuration if it satisfies this constraint
	 * (e.g. if model=foo, only allow configs with bar=1 and baz=2)
	 *
	 * put another way, give a function that will return false if a
	 * configuration should be pruned.
	 *
	 * when counting parameters, constraints are taken in conjunction
	 */
	def addConstraint(f: Map[String, String] => Boolean) {
		constraints += f
		configs == null
	}

	/**
	 * get the parmaters of the i^{th} configuration that
	 * satisfies *all* of the contraints
	 */
	override def getParams(i: Int): Map[String, String] = {
		if(configs == null) genConfigs
		configs(i)
	}

	override def numJobs: Int = {
		if(configs == null) genConfigs
		configs.size
	}

	private def genConfigs {
		configs = (0 until ajh.numJobs).map(ajh.getParams)
			.filter(satisifiesAllConstraints)
			.toIndexedSeq
	}

	private def satisifiesAllConstraints(p: Map[String, String]): Boolean = {
		for(c <- constraints)
			if(!c(p)) return false
		return true
	}
}

/**
 * represents the cross product of the values in the map
 * e.g.
 * setup:
 *    helper.addParam("numTopics", Seq(10, 100, 500))
 *    helper.addParam("dataSet", Seq("nyt.txt", "wsj.txt"))
 * runtime:
 *    helper.getParams(0) = "10", "nyt.txt"
 *    helper.getParams(1) = "10", "wsj.txt"
 *    helper.getParams(2) = "100", "nyt.txt"
 *    helper.getParams(3) = "100", "wsj.txt"
 *    helper.getParams(4) = "500", "nyt.txt"
 *    helper.getParams(5) = "500", "wsj.txt"
 */ 
class ArrayJobHelper {

	// TODO
	// add ability to filter out some configurations
	// (e.g. only range over this parameter when model=Foo)

	// adding ability to order jobs may be too hard
	// if you gave arbitrary scoring functions on job orders, then planning is just as hard as TSP
	// much easier approximation: make the loop over dataset sizes first (so you can get some results on the small dataset first)

	private val paramNames = new ArrayBuffer[String]
	private val paramValues = new HashMap[String, Seq[String]]

	def numJobs: Int = paramValues.values.map(_.size).reduce(_*_)

	def getPossibleValues(paramName: String): Seq[String] = paramValues(paramName)

	/**
	 * earlier calls to this method result in less-embedded loops
	 * put another way, the last call to this will be the first
	 * range of values swept
	 */
	def addParam[T](name: String, values: Seq[T]) {
		paramNames += name
		paramValues += (name -> values.map(_.toString))
	}

	/**
	 * index \in [0, numJobs)
	 * index is the job-id given to you by SGE when you use array jobs
	 */
	private def getParams(index: Int, optionIdx: Int): Map[String, String] = {
		if(optionIdx >= 0) {
			val k = paramNames(optionIdx)
			val values = paramValues(k)
			val v = values(index % values.size)
			getParams(index / values.size, optionIdx - 1) + (k->v)
		}
		else Map[String, String]()
	}

	def getParams(index: Int): Map[String, String] = {
		if(index < 0 || index >= numJobs)
			throw new IllegalArgumentException(helpString())
		getParams(index, paramNames.size-1)
	}

	/**
	 * helper method that takes arguments straight from main,
	 * parses out the index, and prints an error message if one is not provided
	 */
	def getParams(args: Array[String]): Option[Map[String, String]] = {
		try {
			require(args.size == 1)
			val i = args(0).toInt - 1	// sge gives 1-indexed task ids
			require(i >= 0)
			Some(getParams(i))
		} catch {
			case e: Exception =>
				println(helpString())
				None
		}
	}

	def helpString(show: Int = 20): String = {
		val sb = new StringBuilder
		sb.append("jobs setup:\n")
		for(i <- 0 until math.min(show, numJobs))
			sb.append("\t%d: %s\n".format(i+1, getParams(i)))
		if(show < numJobs) sb.append("\t... and %d more\n".format(numJobs-show))
		sb.append("please request %d jobs\n".format(numJobs))
		sb.append("if you wanted to start an array job, you must provide the index of the job\n")
		sb.append("remember that SGE's jobs must be 1-indexed\n")
		sb.append("e.g. qsub -q text.q -t 1-%d acl14-ablation.qsub\n".format(numJobs))
		sb.toString
	}
}

object ArrayJobHelperTests {
	val ajh = new ArrayJobHelper
	ajh.addParam("numTopics", Seq(10, 100, 500))
	ajh.addParam("dataSets", Seq("nyt", "wsj"))
	def main(args: Array[String]) {
		println(ajh.numJobs)
		assert(ajh.numJobs == 6)
		var idx = 0
		for(t <- Seq(10, 100, 500)) {
			for(d <- Seq("nyt", "wsj")) {
				println(ajh.getParams(idx))
				assert(ajh.getParams(idx) == Map("numTopics"->t.toString, "dataSets"->d.toString))
				idx += 1
			}
		}
	}

	// call this before running your array job
	def setup(args: Array[String]) {
		println("request %d slots for your array job".format(ajh.numJobs))
	}

	// call this at the start of your array job
	def run(args: Array[String]) {
		val i = args(0).toInt
		val params = ajh.getParams(i)
		println("running with: " + params)
	}
}


