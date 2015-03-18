// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.inference

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._

object DomainAdaptation {

	val maxDomains = ParmaConfig.getInt("inference.domainAdaptation.maxDomains", 4)
	private[this] val alph = new Alphabet[String]
	val allDomains = "allDomains"
	alph.lookupIndex(allDomains, addIfNotPresent=true)

	def numDomains: Int = alph.size

	/**
	 * domain = None means out of domain, estimates are computed using only
	 * all-domain features (backoff)
	 *
	 * http://acl.ldc.upenn.edu/P/P07/P07-1033.pdf
	 */
	def frustratinglySimple(domain: Option[String], features: SVec): SVec = {

		// domain-independent features
		val sv = SVec.duplicate(features, features.numItems * 2)

		// domain-specific features
		if(!domain.isEmpty) {
			val domainIdx = alph.lookupIndex(domain.get, addIfNotPresent=true)
			if(domainIdx >= maxDomains) {
				throw new RuntimeException("%s is the %d-th domain being used, and you specified".format(domain.get, alph.size) +
					"%d as the maximum number of domains. please sent this to a larger number".format(maxDomains))
			}
			SVec.addWithStride(features, sv, maxDomains, domainIdx)
		}

		sv
	}
}

