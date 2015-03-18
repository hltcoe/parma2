// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features.wordnet;

import edu.jhu.hlt.parma.util.ParmaConfig;

import edu.mit.jwi.*;
import edu.mit.jwi.data.*;
import edu.mit.jwi.item.*;

import java.io.*;
import java.util.*;
import java.util.logging.Logger;

public class WordNet implements Serializable {

	private static final Logger logger = Logger.getLogger(WordNet.class.getName());
	private static WordNet singleton = new WordNet();
	public static final String FEATURES_WORDNET_DATAPATH = "features.wordnet.datapath";

	private transient IRAMDictionary dict;
	private boolean setup = false;
	private int maxDistance = 2;

	public static WordNet getInstance(){
		singleton.setup();
		return singleton;
	}

	public void setup(){
		if(setup) return;
		try {
			File location = ParmaConfig.getDirectory(FEATURES_WORDNET_DATAPATH);
			logger.info("loading wordnet data from " + location.getPath());
			loadDictionary(location.getPath());
			setup = true;
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	private void loadDictionary(String filePath) throws IOException {
		File wnDir = new File(filePath);
		dict = new RAMDictionary(wnDir, ILoadPolicy.IMMEDIATE_LOAD);
    	dict.open();
	}

	/*
 	* Get the synonym distance between two strings. Direct synonyms will return of distance of 1. 
 	* If no synonym path is found, it will return 0.	
 	*/
	public int getSynonymDistance(String first, String second, int maxDistance) {
		assert setup;
		return getDistance(first,second,maxDistance,new String[]{"getSynonyms"});
	}


	/*
 	* Get the hypernym distance between two strings. First argument is a parent, second is child. 
 	* Direct hypernyms will return a distance of 1. If no hypernym path is found, it will return 0.	
 	*/
	public int getHypernymDistance(String parent, String child, int maxDistance) {
		assert setup;
		return getDistance(parent,child,maxDistance,new String[]{"getHypernyms"});
	} 


	/*
 	* Get the hyponym distance between two strings. First argument is a parent, second is child. 
 	* Direct hyponyms will return a distance of 1. If no hyponym path is found, it will return 0.	
 	*/
	public int getHyponymDistance(String parent, String child, int maxDistance) {
		assert setup;
		return getDistance(parent,child,maxDistance,new String[]{"getHyponyms"});
	} 


	/*
 	* Get the meronym distance between two strings. First argument is a parent, second is child.
 	*  Direct meronyms will return a distance of 1. If no meronym path is found, it will return 0.
 	*/
	public int getMeronymDistance(String parent, String child, int maxDistance){
		return getDistance(parent,child, maxDistance,new String[]{"getMeronyms"});
		//return meronymSD.shortestDist(parent, child);
	}


	/*
 	* Get the holonym distance between two strings. First argument is a parent, second is child.
 	*  Direct holonyms will return a distance of 1. If no holonym path is found, it will return 0.
 	*/
	public int getHolonymDistance(String parent, String child, int maxDistance) {
		assert setup;
		return getDistance(parent,child,maxDistance,new String[]{"getHolonyms"});
	} 


	/*
 	* Get the minimum distance between two strings using any relationship.
 	* If no path is found, it will return 0
 	*/
	public int getCombinedDistance(String first, String second, int maxDistance) {
		assert setup;
		return getDistance(first,second,maxDistance,new String[]{"getSynonyms", 
			"getHypernyms", "getHyponyms", "getMeronyms", "getHolonyms"});
	}








	// TODO this appears to work
	// test it, put into features, benchmark it


	public AllPairsShortestPath synSD = new AllPairsShortestPath(maxDistance, new CanSupplyEdges() {
		public Collection<String> outEdges(String word) {
			return getSynonyms(word);
		}
	});

	public AllPairsShortestPath meronymSD = new AllPairsShortestPath(maxDistance, new CanSupplyEdges() {
		public Collection<String> outEdges(String word) {
			return getMeronyms(word);
		}
	});
	
	static interface CanSupplyEdges { public Collection<String> outEdges(String cur); }

	public static class AllPairsShortestPath {
		private Map<String, Map<String, Integer>> knownShortestPaths;
		private Map<String, Collection<String>> adjacency;
		private CanSupplyEdges edgeGetter;
		private int maxDistance;

		public AllPairsShortestPath(int maxDistance, CanSupplyEdges edgeGetter) {
			this.maxDistance = maxDistance;
			knownShortestPaths = new HashMap<String, Map<String, Integer>>();
			adjacency = new HashMap<String, Collection<String>>();
			this.edgeGetter = edgeGetter;
		}

		private Collection<String> leaving(String node) {
			Collection<String> l = adjacency.get(node);
			if(l == null) {
				l = edgeGetter.outEdges(node);
				if(l == null) l = Collections.<String>emptyList();
				adjacency.put(node, l);
			}
			return l;
		}

		public int shortestDist(String source, String dest) {
			return shortestDist(source, dest, maxDistance);
		}

		/**
		 * this is really a lower bound
		 */
		public int shortestDist(String source, String dest, int edgesLeft) {
			
			if(source.equals(dest))
				return 0;

			Integer known = cache(source, dest);
			if(known != null)
				return known;

			if(edgesLeft == 0)
				return 1;	// upper bound

			// check all edges leaving source
			int best = maxDistance;
			for(String next : leaving(source)) {
				int d = 1 + shortestDist(next, dest, edgesLeft-1);
				if(d < best) best = d;
			}
			setShortestKnownPath(source, dest, best);
			return best;
		}

		private void setShortestKnownPath(String source, String dest, int dist) {
			Map<String, Integer> m = knownShortestPaths.get(source);
			if(m == null) {
				m = new HashMap<String, Integer>();
				knownShortestPaths.put(source, m);
			}
			Integer old = m.put(dest, dist);
			assert old == null || old > dist;
		}

		private Integer cache(String source, String dest) {
			Integer d = null;
			Map<String, Integer> m = knownShortestPaths.get(source);
			if(m != null)
				d = m.get(dest);
			return d;
		}
	}





	/*
 	* Performs a breadth first search to find the shortest path, if it exists 
 	*/
	private int getDistance(String first, String second, int maxDistance, String[] methods) {
		assert setup;
		if (first.equalsIgnoreCase(second))
			return 1;		
		Set<String> visitedSet = new HashSet<String>();
		Queue<StringIntPair> queue = new LinkedList<StringIntPair>();
		queue.add(new StringIntPair(first,1));
		visitedSet.add(first);
		while(!queue.isEmpty()) {

			StringIntPair sip = queue.remove();
			if(sip.i == maxDistance)
				return maxDistance;

			Set<String> neighbors = new HashSet<String>();
			for (int i=0; i<methods.length; i++){
				neighbors.addAll(staticDispatch(methods[i], sip.s));
			}

			for(String neighbor : neighbors) {
				if(visitedSet.add(neighbor)) {
					if(neighbor.equals(second))
						return sip.i;
					queue.add(new StringIntPair(neighbor,sip.i+1));
				}
				// if you've already seen the neighbor, then it will be
				// found with a lower cost than this entry could possibly lead to
			}
		}
		return maxDistance + 1;
	}

	private HashSet<String> staticDispatch(String method, String word) {
		if(method.equals("getSynonyms"))
			return getSynonyms(word);
		if(method.equals("getHypernyms"))
			return getHypernyms(word);
		if(method.equals("getHyponyms"))
			return getHyponyms(word);
		if(method.equals("getMeronyms"))
			return getMeronyms(word);
		if(method.equals("getHolonyms"))
			return getHolonyms(word);
		else throw new RuntimeException("wut: " + method);
	}

	/* Get set of synonyms giving an input */
	public HashSet<String> getSynonyms(String input) {
		HashSet<String> set = new HashSet<String>();
		List<ISynset> synsets = getSynsetList(input);
		for (ISynset synset : synsets) {
			for (IWord w : synset.getWords()){
				set.add(formatString(w.getLemma()));
			}
		}
		return set;
	}
	
	/* Get set of hypernyms giving an input */
	public HashSet<String> getHypernyms(String input) {
		return getSetHelper(input, Pointer.HYPERNYM);
	}
	
	/* Get set of hyponyms giving an input */
	public HashSet<String> getHyponyms(String input) {
		return getSetHelper(input, Pointer.HYPONYM);
	}

	/* Get set of meronyms giving an input */
	public HashSet<String> getMeronyms(String input) {
		HashSet<String> set = getSetHelper(input, Pointer.MERONYM_MEMBER);
                set.addAll(getSetHelper(input, Pointer.MERONYM_PART));
                set.addAll(getSetHelper(input,Pointer.MERONYM_SUBSTANCE));
                return set;
	}

	/* Get set of holonyms giving an input */
	public HashSet<String> getHolonyms(String input) {
		HashSet<String> set = getSetHelper(input, Pointer.HOLONYM_MEMBER);
                set.addAll(getSetHelper(input, Pointer.HOLONYM_PART));
                set.addAll(getSetHelper(input,Pointer.HOLONYM_SUBSTANCE));
                return set;
	}

	private HashSet<String> getSetHelper(String input, Pointer pointer) {
		HashSet<String> set = new HashSet<String>();
		List<ISynset> synsets = getSynsetList(input);
		for (ISynset synset : synsets) {
			List<ISynsetID> synsetIds = synset.getRelatedSynsets(pointer);	
			for (ISynsetID sid : synsetIds) {
				for (IWord w : dict.getSynset(sid).getWords()){
					set.add(formatString(w.getLemma()));
				}
			}
		}
		return set;
	}

	private List<ISynset> getSynsetList(String input){
		List<ISynset> synsets = new ArrayList<ISynset>();
		IIndexWord idxWord;
		List<IWordID> wordIDs;
		for (POS pos : POS.values()){
			idxWord = dict.getIndexWord(input, pos);
			if (idxWord != null) {
				wordIDs = idxWord.getWordIDs();
				IWord word;
				for (IWordID wordID : wordIDs){
					word = dict.getWord(wordID);
					synsets.add(word.getSynset());
				}
			}
		}
		return synsets;
	}

	private String formatString(String input) {
		return input.replace('_',' ');
	}

	private class StringIntPair{
		public String s;
		public int i;
		public StringIntPair(String s, int i){
			this.s = s;
			this.i = i;
		}
	}
}
