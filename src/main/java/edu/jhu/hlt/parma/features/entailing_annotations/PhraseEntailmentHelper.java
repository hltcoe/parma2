// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features.entailing_annotations;

import edu.jhu.hlt.parma.util.ParmaConfig;

import java.util.*;
import java.util.logging.Logger;
import java.io.*;


/**
 * Helper class for phrases entailing each other
 *
 *
 * @author Jay DeYoung
 * jdeyoun1@jhu.edu
 */
public class PhraseEntailmentHelper {

	private static final String positiveJudgment = "Yes";
	private static final String negativeJudgment = "No";

	private static final Logger logger = Logger.getLogger(PhraseEntailmentHelper.class.getName());

	private static final String FEATURES_ENTAILING_PHRASE_DATAPATHS = 
			"features.phrase.entailments.datapath";
	private static final String FEATURES_ENTAILING_PHRASE_IGNORE_HEADERS = 
			"features.phrase.entailments.ignore.headers";


	/**
	 * Parses the default files, as determined by the parameters in the config 
	 * file named by FEATURES_ENTAILING_PHRASE_DATAPATHS and 
	 * FEATURES_ENTAILING_PHRASE_IGNORE_HEADERS
	 *
	 *
	 */
	public static PhraseEntailments parseDefaultFiles() {
		logger.info("using default data files");
		String locations, headerIgnores;
		try {
			locations = ParmaConfig.getString(FEATURES_ENTAILING_PHRASE_DATAPATHS);
			headerIgnores = ParmaConfig.getString(FEATURES_ENTAILING_PHRASE_IGNORE_HEADERS);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		String[] files = locations.split(",");
		String[] headerIgnoresArray = headerIgnores.split(",");
		boolean[] discardFirstLine = new boolean[files.length];

		for(int i = 0; i < files.length; i++) {
			discardFirstLine[i] = Boolean.parseBoolean(headerIgnoresArray[i]);
		}

		return parseFiles(files, discardFirstLine);
	}


	/**
	 * @param files the files this should use
	 * @param discardFirstLine whether or not the first line in the file at a 
	 * given index should be discarded (ex: header information). This depends 
	 * entirely on the data.
	 * 
	 */
	public static PhraseEntailments parseFiles(String[] files, 
			boolean[] discardFirstLine) {

		String fileString = "";
		for(int i = 0; i < files.length; i++) {
			fileString += "( " + files[i] + " , ";
			if(discardFirstLine[i]) {
				fileString += "ignoring header line";
			} else {
				fileString += "no header line";
			}
			fileString += ") ";
		}
		logger.info(fileString);
		HashMap<String,HashMap<String,Double>> 
		positive = new HashMap<String, HashMap<String,Double>>(),
		negative = new HashMap<String, HashMap<String,Double>>();	
		BufferedReader buffer;

		try {
			for(int i = 0; i < files.length; i++) {
				buffer = new BufferedReader(new FileReader(files[i]));
				if(discardFirstLine[i]) {
					buffer.readLine();
				}
				while(buffer.ready()) {
					parse(buffer.readLine(), positive, negative);
				}
				buffer.close();
			}
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		return new PhraseEntailments(positive,negative);
	}


	/**
	 * Parses a line from a tab separated file and adds the appropriate columns
	 * to a map
	 *
	 * @param line - a line containing rule_rhs, rule_lhs, lhs, rhs, judgment, 
	 * confidence, tab separated.
	 */
	private static void parse(String line, 
			HashMap<String,HashMap<String,Double>> positive,
			HashMap<String,HashMap<String,Double>> negative) {
		String[] parts = line.split("\t");
		String rule_lhs = parts[0], 
				rule_rhs = parts[1],
				judgment = parts[4].trim(),
				str_confidence = parts[5];

		if(judgment.equalsIgnoreCase(positiveJudgment)) {
			add(positive,rule_lhs,rule_rhs,str_confidence);
		}

		if(judgment.equalsIgnoreCase(negativeJudgment)) {
			add(negative,rule_lhs,rule_rhs,str_confidence);
		}

	}

	/**
	 * Adds a rule_lhs, rule_rhs, confidence to a two dimensional map 
	 * (rule_lhs, rule_rhs) -> confidence
	 *
	 * @param map the map we are adding information to
	 * @param rule_lhs the lhs rule which entails the rhs rule
	 * @param rule_rhs the rhs rule which is entailed (or not entailed) by the lhs
	 * @param str_confidence with some confidence
	 *
	 */
	private static void add(HashMap<String, HashMap<String,Double>> map, 
			String rule_lhs, String rule_rhs, String str_confidence) {
		Double confidence = Double.parseDouble(str_confidence);

		HashMap<String,Double> rhs_conf_map = map.get(rule_lhs);
		if(rhs_conf_map == null) {
			rhs_conf_map = new HashMap<String,Double>();
			map.put(rule_lhs,rhs_conf_map);
		}

		rhs_conf_map.put(rule_rhs,confidence);
	}

}
