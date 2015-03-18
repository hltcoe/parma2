// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util;

import java.util.HashMap;
import java.util.Set;

public class Counter<K> {
	private final HashMap<K, Double> counts = new HashMap<K, Double>();
	private double Z = 0d;
	public void incrementCount(K key) {
		counts.put(key, getCount(key)+1d);
		Z += 1d;
	}
	public int size() { return counts.size(); }
	public boolean containsKey(K key) { return counts.containsKey(key); }
	public double getCount(K key) {
		Double c = counts.get(key);
		assert(c == null || c >= 0d);
		return c == null ? 0d : c;
	}
	public double getProb(K key) {
		return getCount(key) / Z;
	}
	public void setCount(K key, double v) {
		Double c = getCount(key);
		Z -= c;
		Z += v;
		counts.put(key, v);
	}
	public Set<K> keySet() { return counts.keySet(); }
	public double totalCount() {
		assert(Z >= 0d);
		return Z;
	}
}

