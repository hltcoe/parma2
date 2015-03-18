// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder
import java.io.File
import scala.io._
import scala.collection.mutable.{ HashSet, HashMap }

class ReportingVerbs extends AlignmentSimilarity with Logging2 {

	// keys are lemmatized reporting verbs, values are categories (see files)
	var acu = new HashMap[String, String]
	var adelaide = new HashMap[String, String]
	var misc = new HashSet[String]
	
	val acuFile = ParmaConfig.getFile("features.reporting_verbs.acu")
	val adelaideFile = ParmaConfig.getFile("features.reporting_verbs.adelaide")
	val miscFile = ParmaConfig.getFile("features.reporting_verbs.misc")
	
	override def name: String = "ReportingVerbs"
	
	override def setup(docs: java.util.Collection[DocAlignment]) {
	  
		if(!acuFile.exists)
			throw new RuntimeException("can't find acu reporting verbs file: " + acuFile.getPath)
		readReportingVerbFile(acuFile, acu)
		log("read %d acu reporting verbs".format(acu.size))
		assert(acu.size > 0)
		
		if(!adelaideFile.exists)
			throw new RuntimeException("can't find adelaide reporting verbs file: " + adelaideFile.getPath)
		readReportingVerbFile(adelaideFile, adelaide)
		log("read %d adelaide reporting verbs".format(adelaide.size))
		assert(adelaide.size > 0)
		
		misc.clear
		for(line <- Source.fromFile(miscFile, "utf-8").getLines)
			if(line.length > 0) misc += line
		log("read %d misc reporting verbs".format(misc.size))
	}
	
	def isReportingVerb(word: String): Boolean = acu.contains(word) || adelaide.contains(word) || misc.contains(word)
	def getReportingVerbs: Set[String] = acu.keySet.toSet | adelaide.keySet | misc
	
	private def readReportingVerbFile(f: File, putInto: collection.mutable.Map[String, String]) {
		val source = Source.fromFile(f, "utf-8")
		for(line <- source.getLines.filterNot(_.startsWith("#"))) {
			val ar = line.toLowerCase.split("""\t""")
			if(ar.length == 2) {
				val word = ar(1).toLowerCase
				val category = ar(0).toLowerCase.intern
				putInto += (word -> category)
			}
			else warn("unexpected line in acu.txt: " + line)
		}
		source.close
	}
	
	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {
		val (reportCM, passageCM) = CanonicalMentionFinder.canonicalMentions(a, report, passage)
		val rht = report.getHeadToken(reportCM)
		val pht = passage.getHeadToken(passageCM)
		reporting(sv, rht, pht, acu, "acu")
		reporting(sv, rht, pht, adelaide, "adelaide")
	}

	private def reporting(sv: SVec, rTok: Token, pTok: Token, m: collection.Map[String, String], desc: String) {
		val rCat = m.getOrElse(rTok.getLemma.toLowerCase, null)
		val pCat = m.getOrElse(pTok.getLemma.toLowerCase, null)
		if(rCat != null && rCat == pCat)
			b(sv, desc, "categoryMatch")
		if(rCat != null && pCat != null)
			b(sv, desc, "bothAreReporting")
		if(rCat != null ^ pCat != null)
			b(sv, desc, "exactlyOneIsReporting")
	}
	
}


