// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features.entailing_annotations;


/**
 * @author Jay DeYoung
 * jdeyoun1@jhu.edu
 *
 * Basic basic PhraseEntailments object which has three facts per phrase pair (p1, p2):
 * p1 entails p2
 * p1 does not entail p2
 * p1, p2 entailment is not known
 *
 * which form a three-tuple binary feature
 *
 * Assuming your data is consistent, exactly two of the elements of the feature
 * should be false.
 *
 */
import java.util.*;

public class PhraseEntailments {

	private HashMap<String,HashMap<String,Double>> entailed, notEntailed;

	/**
	 * Constructs a PhraseEntailments object based on two maps of entailments
	 */
	public PhraseEntailments(HashMap<String,HashMap<String,Double>> entailed, 
			HashMap<String,HashMap<String,Double>> notEntailed) {
		this.entailed = entailed;
		this.notEntailed = notEntailed;

		System.out.println("entailed size" + entailed.size());
		System.out.println("notEntailed size" + notEntailed.size());
	}

	/**
	 * @return p1 entails p2 or there is no know mapping in entailed for p1.
	 */
	public boolean entailed(String p1, String p2) {
		if(entailed.get(p1) != null) 
			return entailed.get(p1).containsKey(p2);
		return false;
	}

	/**
	 * @return p1 does not entail p2 OR there is no known mapping in notEntailed for p1.
	 */
	public boolean notEntailed(String p1, String p2) {
		if(notEntailed.get(p1) != null)
			return notEntailed.get(p1).containsKey(p2);
		return false;
	}

	/**
	 * @return true iff entailed and notEntailed return false
	 */
	public boolean unknown(String p1, String p2) {
		return !(entailed(p1,p2) || notEntailed(p1,p2));
	}
}
