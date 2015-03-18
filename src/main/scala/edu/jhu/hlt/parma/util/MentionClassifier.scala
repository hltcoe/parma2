// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util

import edu.jhu.hlt.parma.types._
import scala.collection.JavaConversions._

object MentionClassifierUtil {
  
	def getMentionType(document: Document, mention: Mention) = {
		val tok = document.getHeadToken(mention)
		val nerTag = tok.getNerTag
		val posTag = tok.getPosTag 

		if (nerTag.equalsIgnoreCase("DURATION") || nerTag.equalsIgnoreCase("TIME"))
		MentionType.TIME
		else if (posTag.equalsIgnoreCase("NNP") || posTag.equalsIgnoreCase("NNPS"))
		MentionType.NAMED_ENTITY;
		else if (posTag.equalsIgnoreCase("NN") || posTag.equalsIgnoreCase("NNS"))
		MentionType.COMMON_NOUN
		else if (posTag.startsWith("V"))
		MentionType.VERB
		else if (posTag.startsWith("J"))
		MentionType.ADJECTIVE
		MentionType.NONE;
	}
	
}

