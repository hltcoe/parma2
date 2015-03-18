// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder
import edu.jhu.hlt.phylo.distrib.StringEditModel
import edu.jhu.hlt.phylo.lang.AnnotatedString
import java.io.{ File, FileInputStream, ObjectInputStream }

class TransducerSimilarityFeature extends AlignmentSimilarity {

	lazy val peopleModel = deserializeModel(ParmaConfig.getFile("features.transducer.model.people"))
	lazy val organizationsModel = deserializeModel(ParmaConfig.getFile("features.transducer.model.organizations"))
	lazy val locationsModel = deserializeModel(ParmaConfig.getFile("features.transducer.model.locations"))

	private def deserializeModel(from: File): StringEditModel = {
		val ois = new ObjectInputStream(new FileInputStream(from))
		val model = ois.readObject.asInstanceOf[StringEditModel]
		ois.close
		model
	}

	//val oovStringPairProbs = StringPairProbs("oov1", "oov2", Double.NaN, Double.NaN)
	case class StringPairProbs(val x: String, val y: String, val x2y: Double, val y2x: Double) {
		def max: Double = math.max(forwards, backwards)
		def avg: Double = (forwards + backwards) / 2d
		def min: Double = math.min(forwards, backwards)
		def forwards: Double = normalize(x2y)
		def backwards: Double = normalize(y2x)
		def normalize(d: Double): Double =
			if(d.isNaN) -50d
			else if(d.isInfinite && d > 0d) 2d
			else if(d.isInfinite && d < 0d) -60d
			else d
		def >(other: StringPairProbs): Boolean = avg > other.avg
	}

	def probs(model: StringEditModel, x: String, y: String): StringPairProbs = {

		val alph = model.getAlphabet
		AnnotatedString.setAlphabet(alph)

		// remove any OOV characters
		def safen(s: String): String = {
			val sb = new StringBuilder
			var i = 0
			while(i < s.length) {
				val c = s.charAt(i)
				if(alph.lookupIndex(c, false) >= 0)
					sb.append(c)
				i += 1
			}
			sb.toString
		}

		val xx = new AnnotatedString(safen(x))
		val yy = new AnnotatedString(safen(y))
		val x2y = model.logp(xx, yy)
		val y2x = model.logp(yy, xx)
		StringPairProbs(x, y, x2y, y2x)
	}

	private[this] val binarizer = new FixedWidthBinarizer(16, false, -50d, -1d)

	def score(x: String, y: String): Double = {
		val p1 = probs(peopleModel, x, y)
		val p2 = probs(organizationsModel, x, y)
		val p3 = probs(locationsModel, x, y)
		val maxes = Seq(p1.max, p2.max, p3.max)
		maxes.sum / maxes.length
	}

	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {

		val (reportCM, passageCM) = CanonicalMentionFinder.canonicalMentions(a, report, passage)

		// ======================================= head token ============================
		val rh = report.getHeadString(reportCM)
		val ph = passage.getHeadString(passageCM)

		var p = probs(peopleModel, rh, ph)
		var pmax = p
		b(sv, p.max, "transducer-headstring-people-max", binarizer)
		b(sv, p.avg, "transducer-headstring-people-avg", binarizer)

		p = probs(locationsModel, rh, ph)
		if(p > pmax) pmax = p
		b(sv, p.max, "transducer-headstring-locations-max", binarizer)
		b(sv, p.avg, "transducer-headstring-locations-avg", binarizer)

		p = probs(organizationsModel, rh, ph)
		if(p > pmax) pmax = p
		b(sv, p.max, "transducer-headstring-organizations-max", binarizer)
		b(sv, p.avg, "transducer-headstring-organizations-avg", binarizer)

		b(sv, pmax.max, "transducer-headstring-max-max", binarizer)
		b(sv, pmax.avg, "transducer-headstring-max-avg", binarizer)

		// ======================================= full mention ==========================
		val rm = report.getMentionString(reportCM)
		val pm = passage.getMentionString(passageCM)

		p = probs(peopleModel, rm, pm)
		pmax = p
		b(sv, p.max, "transducer-fullmention-people-max", binarizer)
		b(sv, p.avg, "transducer-fullmention-people-avg", binarizer)

		p = probs(locationsModel, rm, pm)
		if(p > pmax) pmax = p
		b(sv, p.max, "transducer-fullmention-locations-max", binarizer)
		b(sv, p.avg, "transducer-fullmention-locations-avg", binarizer)

		p = probs(organizationsModel, rm, pm)
		if(p > pmax) pmax = p
		b(sv, p.max, "transducer-fullmention-organizations-max", binarizer)
		b(sv, p.avg, "transducer-fullmention-organizations-avg", binarizer)

		b(sv, pmax.max, "transducer-fullmention-max-max", binarizer)
		b(sv, pmax.avg, "transducer-fullmention-max-avg", binarizer)

	}
}

