// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.inference

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.diagnostics.GeneralDiagnostics
import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap
import java.util.logging.Logger

object CanonicalMentionFinder {

	private val nerProperNouns = Set("ORGANIZATION", "LOCATION", "PERSON")

  	def scoreCanonicalMention(doc: Document, m: Mention): Double = {

		//println("[CanonicalMentionFinder scoreCanonicalMention] doc=%s m.start=%d m.head=%d m.end=%d"
		//	.format(doc.getDocId, m.getStartTokenIdx, m.getHeadTokenIdx, m.getEndTokenIdx))
		
		// if the mention is too long, then it is not a valid canonical mention
		val tokens = doc.getMentionTokens(m)
		val lengthPenalty = tokens
				.filterNot(_.getWord matches """\W""")	// not counting punctuation
				.filterNot(_.getPosTag.startsWith("D"))	// not counting determiners
				.filterNot(_.getPosTag.startsWith("POS"))	// not counting possesives ("'s", not "his" which is PRP)
				.size match {
			case 0 => 8d
			case 1 => 0d
			case 2 => 0d
			case x => math.pow(x - 3, 1.1) / 3d
		}

		// prefer mentions near the beginning in ties
		val positionPenalty = 0.01d * m.getSentenceIdx + 0.0001d * m.getHeadTokenIdx

		val nerBonus = math.min(3, tokens.filter(t => nerProperNouns.contains(t.getNerTag)).size)
		val posBonus = math.min(3, tokens.filter(t => t.getPosTag.startsWith("NNP")).size)

		val headPosBonus = doc.getHeadToken(m).getPosTag match {
			case "NNP" => 1d
			case "NNPS" => 1d
			case "PRP" => -5d
			case "PRP$" => -5d
			case _ => 0d
		}
		val headNer = doc.getHeadToken(m).getNerTag
		val headNerBonus = 
			if(nerProperNouns.contains(headNer))
				1d
			else if(headNer equalsIgnoreCase "O")
				-5d
			else
				0d
		tokens.size -lengthPenalty -positionPenalty +nerBonus +posBonus +headPosBonus +headNerBonus
	}
	
	def canonicalMentions(a: Alignment, c: Context): (Mention, Mention) = canonicalMentions(a, c.report, c.passage)
	def canonicalMentions(a: Alignment, report: Document, passage: Document): (Mention, Mention) = a match {
		case pa: PredicateAlignment => (pa.reportPred.location, pa.passagePred.location)
		case aa: ArgCorefAlignment => {
			if(aa.cmCache == null) {
				val reportArg = canonicalMention(aa.reportCoref, report)
				val passageArg = canonicalMention(aa.passageCoref, passage)
				aa.cmCache = (reportArg.location, passageArg.location)
			}
			aa.cmCache
		}
	}
	
	def canonicalMention(ac: ArgumentCoref, doc: Document): Argument = {
		if(ac.size == 0)
			throw new RuntimeException("no canonical mention in empty chain!")

		val scoredMentions = ac.map(arg => (arg, scoreCanonicalMention(doc, arg.location)))
		val argument: Argument = scoredMentions.maxBy(_._2)._1

		/* right now these seem to hurt not help
		// check special cases
		if(doc.getHeadToken(argument).getNerTag equalsIgnoreCase "O") {
			val newMention = namedEntityFinder(doc, argument)
			new Argument(newMention, argument.getCorefSet)
		}	// TODO implement other cases for canonical mention (other than NER)
		else argument
		*/
		argument
	}

	private def namedEntityFinder(doc: Document, arg: Argument): Mention = {
		
		// Extract out the span of the NER.
		// Find the left boundary.
		val nerType : String = doc.getHeadToken(arg).getNerTag
		val nerTags = doc.getSentence(arg).tokens.map(_.getNerTag)
		val m = arg.location
		val h = m.getHeadTokenIdx
		val s = m.getStartTokenIdx
		val e = m.getEndTokenIdx

		val right = (h until e).takeWhile(nerTags(_).equalsIgnoreCase(nerType)).last
		val left = (h to s by -1).takeWhile(nerTags(_).equalsIgnoreCase(nerType)).last

		val am = MentionBuilder.from(m.getSentenceIdx, left, right+1, h)
		GeneralDiagnostics.checkMention(am, doc)
		am
	}
	
}

