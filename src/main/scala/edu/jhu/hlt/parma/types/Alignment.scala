// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.types

import scala.collection.JavaConversions._

// NOTE: an alignment only represents an positive alignment, either hypothetical or real
// DO NOT case class NullPredicateAlignment(val pred: Predicate, val inReport: Boolean)
	// then all the classes that match alignments will have to change
// for MaxMarginHAM, you only need feature vectors for null alignments

abstract class Alignment {
	override def toString: String = {
		this match {
			case pa: PredicateAlignment =>
				"(PredicateAlignment report=%s, passage=%s)"
			  		.format(pa.reportPred, pa.passagePred)
			case aa: ArgCorefAlignment =>
				"(ArgCorefAlignment report=%s, passage=%s)"
			  		.format(aa.reportCoref, aa.passageCoref)
		}
	}
	
	def unionGovernedBy(report: Document, passage: Document): (Seq[Dependency[Token]], Seq[Dependency[Token]]) = {
		this match {
			case aca: ArgCorefAlignment => {
				val r = aca.reportCoref.flatMap(arg => {
					report.governedBy(arg.location)
				})
				val p = aca.passageCoref.flatMap(arg => {
					passage.governedBy(arg.location)
				})
				(r.toSeq, p.toSeq)
			}
			case pa: PredicateAlignment => {
				val r = report.governedBy(pa.reportPred.location)
				val p = passage.governedBy(pa.passagePred.location)
				(r.toSeq, p.toSeq)
			}
		}
	}
	
	def unionGoverns(report: Document, passage: Document): (Seq[Dependency[Token]], Seq[Dependency[Token]]) = {
		this match {
			case aca: ArgCorefAlignment => {
				val r = aca.reportCoref.flatMap(arg => {
					report.governs(arg.location)
				})
				val p = aca.passageCoref.flatMap(arg => {
					passage.governs(arg.location)
				})
				(r.toSeq, p.toSeq)
			}
			case pa: PredicateAlignment => {
				val r = report.governs(pa.reportPred.location)
				val p = passage.governs(pa.passagePred.location)
				(r.toSeq, p.toSeq)
			}
		}
	}
}

case class PredicateAlignment(val reportPred: Predicate, val passagePred: Predicate) extends Alignment {
	override def hashCode: Int = (reportPred.hashCode << 16) | passagePred.hashCode
	override def equals(o: Any): Boolean = {
		if(o.isInstanceOf[PredicateAlignment]) {
			val other = o.asInstanceOf[PredicateAlignment]
			reportPred == other.reportPred && passagePred == other.passagePred
		}
		else false
	}
}

case class ArgCorefAlignment(val reportCoref: ArgumentCoref, val passageCoref: ArgumentCoref) extends Alignment {
	override def hashCode: Int = (reportCoref.hashCode << 16) | passageCoref.hashCode
	override def equals(o: Any): Boolean = {
		if(o.isInstanceOf[ArgCorefAlignment]) {
			val other = o.asInstanceOf[ArgCorefAlignment]
			reportCoref == other.reportCoref && passageCoref == other.passageCoref
		}
		else false
	}

	// allows CanonicalMentionFinder to cache
	var cmCache: (Mention, Mention) = null
}

