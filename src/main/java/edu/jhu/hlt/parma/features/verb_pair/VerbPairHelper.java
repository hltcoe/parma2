// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features.verb_pair;

import edu.jhu.hlt.parma.util.ParmaConfig;

import java.util.*;
import java.io.*;
import java.util.logging.Logger;

/**
 * Helper class for verb pairs and their corresponding entailments
 * Provides methods for constructing a VerbEntailments object from a file, spec:
 * v1,v2,{1,0}
 * where {1,0} indicates entailed (1) or not entailed (0).
 *
 * @author Jay DeYoung
 * jdeyoun1@jhu.edu
 *
 */
public class VerbPairHelper {


	private static String notEntailedKey="0", entailedKey="1";
	private static final String DELIM = ",";

	private static final Logger logger = Logger.getLogger(VerbPairHelper.class.getName());
	private static final String FEATURES_VE_ANNOTATED_DATAPATH = "features.verbentailmentannotated.datapath";


	/**
	 * Constructs a VerbEntailments object from the entailments defined by the value of
	 * FEATURES_VE_ANNOTATED_DATAPATH as defined in parma.config
	 *
	 */
	public static VerbEntailments constructEntailments() {
		logger.info("Using default data file from config");
		String location;
		try {
			location = ParmaConfig.getString(FEATURES_VE_ANNOTATED_DATAPATH);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		return constructEntailments(location);
	}


	/**
	 * Input - a file of verb entailments
	 *
	 * Output - VerbEntailments
	 *
	 *
	 * @param filepath - path to a csv file formatted as v1,v2,{0,1}
	 * @return a VerbEntailments class that contains the mappings of v1 to v2, their relative entailments, and whether or not they were entailed.
	 */
	public static VerbEntailments constructEntailments(String filepath)  {
		logger.info("loading verb entailment data from " + filepath);
		HashMap<String,HashSet<String>> entailed = 
				new HashMap<String, HashSet<String>>(), 
				notEntailed = new HashMap<String, HashSet<String>>();
		parseFile(entailed,notEntailed,filepath);
		return new VerbEntailments(entailed,notEntailed);
	}


	/**
	 * Input - two maps from v1 to a set of all v2, one for explicit entailments,
	 * one for explicit non-entailments, and a filepath
	 *
	 * Output - nothing, all modifications are made to the maps.
	 *
	 *
	 */
	private static void parseFile(HashMap<String,HashSet<String>> entailed,
			HashMap<String, HashSet<String>> notEntailed, String filepath) {
		try { 
			BufferedReader buffer = new BufferedReader(new FileReader(filepath));
			while(buffer.ready()) {
				parseLine(entailed, notEntailed, buffer.readLine());
			}
			buffer.close();
		} catch (IOException e) {
			throw new RuntimeException(e);
		} 
	}

	/**
	 * parses a line of formatted as v1,v2,{0,1}
	 * where {0,1} indicates the v1 does not entail v2 or v1 does entail v2, respectively,
	 * and adds this entailment to the appropriate map.
	 * @param entailed a map of a verb to a set of the verbs it entails, possibly empty
	 * @param notEntailed a map of a verb to a set of the verbs it does not entail, possibly empty
	 * @param line a line to parse and add to the appropriate set
	 */
	private static void parseLine(HashMap<String,HashSet<String>> entailed, 
			HashMap<String, HashSet<String>> notEntailed, 
			String line) {
		String[] parts = line.split(DELIM);
		String verba = parts[0];
		String verbb = parts[1];
		String entailmentRelation = parts[2];
		//handle the positive cases
		if(entailmentRelation.equals(entailedKey)) {
			HashSet<String> entailedVerbs = entailed.get(verba);
			if(entailedVerbs == null) {
				entailedVerbs = new HashSet<String>();
				entailed.put(verba,entailedVerbs);
			}
			entailedVerbs.add(verbb);
		}
		//handle the negative cases
		if(entailmentRelation.equals(notEntailedKey)) {
			HashSet<String> notEntailedVerbs = notEntailed.get(verba);
			if(notEntailedVerbs == null) {
				notEntailedVerbs = new HashSet<String>();
				notEntailed.put(verba,notEntailedVerbs);
			}
			notEntailedVerbs.add(verbb);
		}
	}
}
