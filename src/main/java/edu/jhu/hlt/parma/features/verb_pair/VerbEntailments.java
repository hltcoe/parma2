// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features.verb_pair;

import java.util.*;

/**
 * @author Jay DeYoung
 * jdeyoun1@jhu.edu
 *
 * Basic basic VerbEntailments object which has three facts per verb pair (v1, v2):
 * whether or not v1 entails v2
 * whether or not v1 does not entail v2
 * whether or not the entailment of v1 and v2 is known. 
 *
 * which form a three-tuple feature
 *
 * Assuming your data is consistent, exactly two of the elements of the feature
 * should be false.
 *
 */
public class VerbEntailments {

	private HashMap<String,HashSet<String>> entailed, notEntailed;

	/**
	 * Constructs a VerbEntailments object based on two maps of entailments
	 */
	public VerbEntailments(HashMap<String,HashSet<String>> entailed, 
			HashMap<String,HashSet<String>> notEntailed) {
		this.entailed = entailed;
		this.notEntailed = notEntailed;
	}

	/**
	 * @return v1 entails v2 or there is no know mapping in entailed for v1.
	 */
	public boolean entailed(String v1, String v2) {
		if(entailed.get(v1) != null) 
			return entailed.get(v1).contains(v2);
		return false;
	}

	/**
	 * @return v1 does not entail v2 OR there is no known mapping in notEntailed for v1.
	 */
	public boolean notEntailed(String v1, String v2) {
		if(notEntailed.get(v1) != null)
			return notEntailed.get(v1).contains(v2);
		return false;
	}

	/**
	 * @return true iff entailed and notEntailed return false
	 */
	public boolean unknown(String v1, String v2) {
		return !(entailed(v1,v2) || notEntailed(v1,v2));
	}

}
