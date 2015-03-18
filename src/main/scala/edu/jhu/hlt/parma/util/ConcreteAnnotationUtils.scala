// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.concrete.Concrete._
import edu.jhu.hlt.concrete.util._
import collection.mutable.ArrayBuffer
import collection.JavaConversions._
import java.io.DataOutputStream
import java.util.Calendar

object ConcreteAnnotationUtils extends Logging2 {
	
	var verbose = false

	val annotationTime = Calendar.getInstance.getTimeInMillis / 1000
	val meta = AnnotationMetadata.newBuilder
		.setTimestamp(annotationTime)
		.setTool("parma")
		.setConfidence(1f) 
		.build
	
	def defaultMetadata = meta

	def isParmaMetadata(am: AnnotationMetadata): Boolean = {
		val t = am.getTool
		t != null && t == "parma"
	}

	/**
	 * parma only wants to operate on one hypothesis from Concrete
	 * this class specifies one hypothesis about a Communication.
	 */
	class SingleCommunicationAnnotation(
		val ems: EntityMentionSet,
		val es: EntitySet,
		val sms: SituationMentionSet,
		val ss: SituationSet)

	/**
	 * strict makes sure that there is only one parma annotation,
	 * and throws an exception if there isn't. if not strict, it
	 * chooses the last parma annotation.
	 */
	def getParmaAnnotations(c: Communication, strict: Boolean = true): SingleCommunicationAnnotation = {

		// parmas annotations
		val emsl = c.getEntityMentionSetList.filter(x => isParmaMetadata(x.getMetadata))
		val esl = c.getEntitySetList.filter(x => isParmaMetadata(x.getMetadata))
		val smsl = c.getSituationMentionSetList.filter(x => isParmaMetadata(x.getMetadata))
		val ssl = c.getSituationSetList.filter(x => isParmaMetadata(x.getMetadata))

		if(strict && (emsl.size > 1 || esl.size > 1 || smsl.size > 1 || ssl.size > 1))
			throw new RuntimeException

		// default to any other annotations if parmas are not available
		val ems =
			if(emsl.size > 0) {
				if(strict && emsl.size > 1) throw new RuntimeException
				emsl.last
			}
			else c.getEntityMentionSetList.last

		val es =
			if(esl.size > 0) {
				if(strict && esl.size > 1) throw new RuntimeException
				esl.last
			}
			else c.getEntitySetList.last

		val sms =
			if(smsl.size > 0) {
				if(strict && smsl.size > 1) throw new RuntimeException
				smsl.last
			}
			else c.getSituationMentionSetList.last

		val ss =
			if(ssl.size > 0) {
				if(strict && ssl.size > 1) throw new RuntimeException
				ssl.last
			}
			else c.getSituationSetList.last

		new SingleCommunicationAnnotation(ems, es, sms, ss)
	}

	def getParmaSentenceSegmentation(section: Section, strict: Boolean = false): SentenceSegmentation = {
		if(section.getSentenceSegmentationCount == 0)
			throw new RuntimeException("there are no SentenceSegmentations to choose from")
		if(section.getSentenceSegmentationCount == 1)
			section.getSentenceSegmentation(0)
		else {
			val parmas = section.getSentenceSegmentationList
				.filter(ss => isParmaMetadata(ss.getMetadata))
			if(parmas.size == 0) {
				warn("there are many SentenceSegmentations, but none were made by parma")
				if(strict) throw new RuntimeException
				warn("using the last one")
				section.getSentenceSegmentationList.last
			}
			else if(parmas.size == 1) parmas(0)
			else {
				warn("there are many parma SentenceSegmentations (why???)")
				warn("using the last one")
				parmas.last
			}
		}
	}

}

