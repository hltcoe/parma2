// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util

import scala.collection.mutable.HashMap
import java.util.Date
import java.io._

object Profiler extends Logging2 {
	
	val PROFILE_FILE = "diagnostics.profile.file"
		
	sealed class Stats(val millis: Long, val count: Long) {
		def +(s: Stats) =
			new Stats(millis + s.millis, count + s.count)
		def seconds = millis / 1000d
		def minutes = seconds / 60d
		def hours = minutes / 60d
		def days = hours / 24d
	}
	
	private val starts = new HashMap[String, Stats]
	private val totals = new HashMap[String, Stats]
	
	def getTime[T](block: => T, seconds: Boolean = true): Double =
		getTimeAndValue(block, seconds)._1

	def getTimeAndValue[T](block: => T, seconds: Boolean = true): (Double, T) = {
		val start = System.currentTimeMillis
		val rvalue = block
		val time = (System.currentTimeMillis - start) / (if(seconds) 1000d else 1d)
		(time, rvalue)
	}

	// TODO this design is bad, if you have something like
	// Profiler.startTask("foo")
	// ... code in which foo crashes ...
	// Profiler.endTask("foo")
	// and you want to handle the exception and go on, you will mess up the state of the Profiler
	// TODO also Profiler should be a class not an object! make a global Profiler available though

	/**
	 * effectively calles endTask() for all outstanding tasks being timed
	 */
	def clearStartTimes { starts.clear }

	// man... i really need to make this not a singleton...
	def clearTimes { totals.clear }
	
	def time[T](taskName: String, block: Unit => T): T = {
		if(starts.contains(taskName))
		throw new RuntimeException("you can only have one task named \"%s\" running at once!".format(taskName))
		val s = System.currentTimeMillis
		val r = block()
		val cur = new Stats(System.currentTimeMillis - s, 1)
		val old = totals.getOrElse(taskName, new Stats(0, 0))
		totals.put(taskName, cur + old)
		r
	}
	
	/**
	 * returns the time this task started
	 */
	def startTask(taskName: String): Long = {
		val now = new Stats(System.currentTimeMillis, 0l)
		starts.put(taskName, now) match {
			case Some(s) => throw new RuntimeException("you can only have one task named \"%s\" running at once!".format(taskName))
			case None => {}
		}
		now.millis
	}
	
	/**
	 * returns the time taken, in milliseconds
	 */
	def endTask(taskName: String): Long = {
		starts.remove(taskName) match {
			case Some(s) => {
			val taken = System.currentTimeMillis - s.millis
				val cur = new Stats(taken, 1)
				val old = totals.getOrElse(taskName, new Stats(0, 0))
				totals.put(taskName, cur + old)
				taken
			}
			case None => throw new RuntimeException("you never started task \"%s\"!".format(taskName))
		}
	}
	
	/**
	 * returns time in sections
	 */
	def timeForTask(taskName: String): Double = {
		totals.getOrElse(taskName, new Stats(0,0)).seconds
	}
	
	def writeoutTimes {
		val profilePath = ParmaConfig.getString(PROFILE_FILE, null)
		if(profilePath != null)
			Profiler.writeoutTimes(new File(profilePath))
		else
			warn("cannot writeout profile times because no file was specified in parma.config with " + PROFILE_FILE)
	}
	
	def writeoutTimes(f: File) {
		val fos = new FileOutputStream(f)
		writeoutTimes(fos)
		fos.close
	}

	def writeoutTimes(os: OutputStream) {
		val ps = new PrintStream(os)
		ps.println(new Date toString)
		for((task, stats) <- totals.toList.sortBy(_._2.seconds).reverse)
			ps.println("%-60s %-10s %.1f seconds".format(task, stats.count, stats.seconds))
	}
}

