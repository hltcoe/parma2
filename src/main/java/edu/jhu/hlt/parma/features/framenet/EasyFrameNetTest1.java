// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features.framenet;

import java.util.*;
import static java.util.Arrays.asList;

public class EasyFrameNetTest1{

	private static EasyFrameNetTest1 singleton = new EasyFrameNetTest1();

	public static EasyFrameNetTest1 getInstance(){
		return singleton;
	}

	// public static void main(String[] args) {
	// String rpred = args[0];
	// String dpred = args[1];

	// Set<String> frames = predicatesToFrames.get(rpred);
	// Iterator rf = frames.iterator();
	// while (rf.hasNext()) {
	// // String frame = ((String) rf.next());
	// Object frame = rf.next();
	// System.out.print(rpred + " frames include: ");
	// System.out.println(frame);
	// Set<String> synset = framesToPredicates.get(frame);
	// System.out.print(rpred + " synonyms include: ");
	// System.out.println(synset);
	// if (synset.contains(dpred)) {
	// System.out.println(dpred + " is a synonym of " + rpred);
	// }
	// }

	// }
	private static Map<String, Set<String>> predicatesToFrames;
	private static Map<String, Set<String>> framesToPredicates;
	private static Map<String, Set<ArrayList<String>>> framesToRoles;
	private static Map<String, Set<ArrayList<String>>> predicatesToValences;

	static{
		predicatesToFrames=new LinkedHashMap<String,Set<String>>();
		initPredToFrame();
		framesToPredicates=new LinkedHashMap<String,Set<String>>();
		initFrameToPred();
		framesToRoles=new LinkedHashMap<String,Set<ArrayList<String>>>();
		initFramesToRoles();
		predicatesToValences=new LinkedHashMap<String,Set<ArrayList<String>>>();
		initPredToValences();
	}

	private static void add(Map<String, Set<String>> hash, String key, String val){
		if(hash.get(key) == null) {
			hash.put(key, new HashSet<String>());
		}
		hash.get(key).add(val);
	}

	private static void add(Map<String, Set<ArrayList<String>>> hash, String key, ArrayList<String> val){
		if(hash.get(key) == null) {
			hash.put(key, new HashSet<ArrayList<String>>());
		}
		hash.get(key).add(val);
	}

	public boolean  sharedFrame(String first, String second) {
		Set<String> frames = predicatesToFrames.get(first);
		for(Object frame : frames) {
			System.out.print(first + " frames include: ");
			System.out.println(frame);
			Set<String> synset = framesToPredicates.get(frame);
			System.out.print(first + " synonyms include: ");
			System.out.println(synset);
			if (synset.contains(second)) {
				System.out.println(second + " is a synonym of " + first);
				return true;
			}
		}
		return false;
	}

	private static void initPredToFrame(){ 

		add(predicatesToFrames, "cause", "Causation");

	}

	private static void initFrameToPred() {

		add(framesToPredicates, "Causation", "cause");
		add(framesToPredicates, "Causation", "cause");
		add(framesToPredicates, "Causation", "make");
		add(framesToPredicates, "Causation", "lead_(to)");
		add(framesToPredicates, "Causation", "reason");
		add(framesToPredicates, "Causation", "send");
		add(framesToPredicates, "Causation", "bring_about");
		add(framesToPredicates, "Causation", "precipitate");
		add(framesToPredicates, "Causation", "causative");
		add(framesToPredicates, "Causation", "render");
		add(framesToPredicates, "Causation", "bring");
		add(framesToPredicates, "Causation", "bring on");
		add(framesToPredicates, "Causation", "induce");
		add(framesToPredicates, "Causation", "wreak");
		add(framesToPredicates, "Causation", "put");
		add(framesToPredicates, "Causation", "since");
		add(framesToPredicates, "Causation", "because");
		add(framesToPredicates, "Causation", "because of");
		add(framesToPredicates, "Causation", "raise");
		add(framesToPredicates, "Causation", "result_(in)");
		add(framesToPredicates, "Causation", "mean");
		add(framesToPredicates, "Causation", "result");
		add(framesToPredicates, "Causation", "leave");
		add(framesToPredicates, "Causation", "for");
		add(framesToPredicates, "Causation", "see");
		add(framesToPredicates, "Causation", "force");
		add(framesToPredicates, "Causation", "give rise");
		add(framesToPredicates, "Causation", "so");

	}
	private static void initFramesToRoles(){

		add(framesToRoles, "Causation", new ArrayList<String>(asList("Place", "Locative_relation")));
		add(framesToRoles, "Causation", new ArrayList<String>(asList("Reason", "State_of_affairs")));
		add(framesToRoles, "Causation", new ArrayList<String>(asList("Time", "Time")));
		add(framesToRoles, "Causation", new ArrayList<String>(asList("Actor", "Sentient")));
		add(framesToRoles, "Causation", new ArrayList<String>(asList("Concessive", "Support")));
		add(framesToRoles, "Causation", new ArrayList<String>(asList("Concessive", "Support")));

	}

	private static void initPredToValences(){

		add(predicatesToValences, "cause", new ArrayList<String>(asList("Actor", "Cause", "Effect")));
		add(predicatesToValences, "cause", new ArrayList<String>(asList("Actor", "Effect")));
		add(predicatesToValences, "cause", new ArrayList<String>(asList("Actor", "Effect", "Effect")));
		add(predicatesToValences, "cause", new ArrayList<String>(asList("Actor", "Effect", "Place")));
		add(predicatesToValences, "cause", new ArrayList<String>(asList("Actor", "Effect", "Place", "Time")));
		add(predicatesToValences, "cause", new ArrayList<String>(asList("Actor", "Effect", "Time")));
		add(predicatesToValences, "cause", new ArrayList<String>(asList("Affected", "Cause", "Effect")));
		add(predicatesToValences, "cause", new ArrayList<String>(asList("Cause", "Cause", "Effect")));
		add(predicatesToValences, "cause", new ArrayList<String>(asList("Cause", "Effect")));
		add(predicatesToValences, "cause", new ArrayList<String>(asList("Cause", "Effect", "Effect")));
		add(predicatesToValences, "cause", new ArrayList<String>(asList("Cause", "Effect", "Place", "Time")));
		add(predicatesToValences, "cause", new ArrayList<String>(asList("Cause", "Effect", "Time")));

	}
}


