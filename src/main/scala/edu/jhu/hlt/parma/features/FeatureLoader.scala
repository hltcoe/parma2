// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.util.ParmaConfig
import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.inference._
import scala.collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer

object FeatureLoader {
  
    val FEATURES = "features"
	val FEATURES_TO_ABLATE = "features.ablate"
	val FEATURES_SPLIT_PREDARG = "features.split.predarg"
	val FEATURES_SPLIT_CLASSIFICATION = "features.split.classifier"

	private def getFullFeatureName(classname: String): String = {
		if(classname.startsWith("edu.jhu.hlt.parma")) classname
		else "edu.jhu.hlt.parma.features."+classname
	}
    
    def loadFeatureFunction(classname: String): AlignmentSimilarity = {
		val fullname = getFullFeatureName(classname)
		val inst = Class.forName(fullname).newInstance
		var ret: AlignmentSimilarity = null
		try {
			ret = inst.asInstanceOf[AlignmentSimilarity]
		}
		catch {
			case e: Exception =>
				throw new RuntimeException(e)
		}
		if(ret == null)
			throw new RuntimeException("couldn't load " + fullname)
		ret
    }
    
    // "ppdb" is now just one feature
    // you can split it by doing "features.split.classifier = ppdb"
    // to split feature on whether it is a predicate or argument, "featuers.split.predarg = ppdb"
    // both of these parma.config keywords can take a list of features
    
    private def getSplitSet(keyword: String): Set[String] = {
		val splitAll = ParmaConfig.getString(keyword, "").equalsIgnoreCase("ALL")
		if(splitAll) new Set[String] {
			override def contains(s: String) = true
			override def iterator = { throw new RuntimeException("dont call me") }
			override def +(elem: String) = { throw new RuntimeException("dont call me") }
			override def -(elem: String) = { throw new RuntimeException("dont call me") }
		}
		else ParmaConfig.getStrings(keyword, Array()).map(getFullFeatureName).toSet
    }
    
	def getFeatures(alph: Alphabet[String]): Seq[AlignmentSimilarity] = {
		var load = ParmaConfig.getStrings(FEATURES).map(getFullFeatureName)
		val ablate = ParmaConfig.getStrings(FEATURES_TO_ABLATE, Array()).map(getFullFeatureName)
		if(ablate.size > 0) {
		    // load each of the ablated features just to make
			// sure there wasn't a typo (and accidentally include
			// a feature that should have been ablated)
		    ablate.foreach(a => {
		    	val aa = loadFeatureFunction(a)
		    	println("ablating feature " + a)
		    })
			load = load.filterNot(ablate.contains(_))
		}
		else println("[FeatureLoader] no features were selected for ablation")
		
		val predArgSplit = getSplitSet(FEATURES_SPLIT_PREDARG)	
		val classificationSplit = getSplitSet(FEATURES_SPLIT_CLASSIFICATION)
		
		val all = load.flatMap(fn => {
			val f: AlignmentSimilarity = loadFeatureFunction(fn)
			lazy val g = {
				println("[FeatureLoader] promoting %s with MentionClassifierRefinement".format(fn))
				new FeatureFunctionRefiner(f, new MentionClassifierRefinement)
			}
			lazy val h = {
				println("[FeatureLoader] promoting %s with AlignmentTypeRefinement".format(fn))
				new FeatureFunctionRefiner(f, new AlignmentTypeRefinement)
			}
			if(predArgSplit.contains(fn) && classificationSplit.contains(fn))
				List(g, h)
			else if(predArgSplit.contains(fn)) List(h)
			else if(classificationSplit.contains(fn)) List(g)
			else List(f)
		})
		println("[FeatureLoader] loaded %d features total".format(all.size))
		all.foreach(_.setAlphabet(alph))
		all
	}
	
}

