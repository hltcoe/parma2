package edu.jhu.hlt.parma.evaluation;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * Computes a bootstrap estimate of whether or not one system is better than another.
 * You provide scores for both systems on test instances, and this class does the bootstrap
 * simulations. You can also provide weights for score functions that are not a simple
 * average of the instances' scores.
 * 
 * @author travis
 */
public class PairedBootstrap {

	/** print a warning if it looks like you might have mixed up the order of the pairs */
	public boolean verbose = false;

	private double[] deltaScores;
	private double[] weights;
	private double avg;

	/**
	 * computes the one-sided bootstrap p-value that both systems
	 * have the same performance.
	 * @param numSamples is how many bootstrap samples to compute
	 * @param r does the random draws
	 * @return p-value (lower values are more significant)
	 */
	public double pValue(int numSamples, Random r) {
		final int n = deltaScores.length;
		int betterSystemWins = 0;
		for(int i=0; i<numSamples; i++) {
			
			// bootstrap estimate of the average score
			double d = 0d;
			for(int j=0; j<n; j++) {
				int idx = r.nextInt(n);
				double w = weights == null ? 1d : weights[idx];
				d += deltaScores[idx] * w;
			}

			if(d > 0d)
				betterSystemWins++;
		}
		return 1d - ((double) betterSystemWins) / numSamples;
	}

	
	public double avgGain() { return avg; }

	public PairedBootstrap(List<Double> betterSystemsGain) {
		this(betterSystemsGain, null);
	}

	public PairedBootstrap(List<Double> betterSystemsGain, List<Double> weights) {
		int n = betterSystemsGain.size();
		if(weights != null && n != weights.size())
			throw new IllegalArgumentException();
		this.avg = 0d;
		double z = 0d;
		this.deltaScores = new double[n];
		this.weights = new double[n];
		for(int i=0; i<n; i++) {
			this.deltaScores[i] = betterSystemsGain.get(i);
			this.weights[i] = weights == null ? 1d : weights.get(i);
			if(this.weights[i] < 0d)
				throw new IllegalArgumentException();
			this.avg += deltaScores[i] * this.weights[i];
			z += this.weights[i];
		}
		this.avg /= z;
		if(verbose && avg < 0d)
			System.err.println("[PairedBootstrap] did you get the signs backwards? avg=" + avg);
	}
	
	public PairedBootstrap(List<Double> betterSystemScores, List<Double> worseSystemScores, List<Double> weights) {
		this(getDiff(betterSystemScores, worseSystemScores), weights);
	}
	
	public static List<Double> getDiff(List<Double> betterSystemScores, List<Double> worseSystemScores) {
		int n = betterSystemScores.size();
		if(n != worseSystemScores.size())
			throw new IllegalArgumentException();
		List<Double> diff = new ArrayList<Double>();
		for(int i=0; i<n; i++)
			diff.add(betterSystemScores.get(i) - worseSystemScores.get(i));
		return diff;
	}
	

	public static class MutablePairedBootstrap {
		private List<Double> deltaScores = new ArrayList<Double>();
		private List<Double> weights;
		public void accum(double betterSystemScore, double worseSystemScore, double weight) {
			deltaScores.add(betterSystemScore - worseSystemScore);
			if(weights == null)
				weights = new ArrayList<Double>();
			weights.add(weight);
		}
		public void accum(double deltaScore, double weight) {
			deltaScores.add(deltaScore);
			if(weights == null)
				weights = new ArrayList<Double>();
			weights.add(weight);
		}
		public void accum(double deltaScore) {
			deltaScores.add(deltaScore);
		}
        public double pValue(int numSamples, Random r) {
        	if(weights != null && deltaScores.size() != weights.size())
        		throw new IllegalStateException("you have to consistently either use weights or not");
        	PairedBootstrap pbs = new PairedBootstrap(deltaScores, weights);
        	return pbs.pValue(numSamples, r);
        }
        public double avgGain() {
        	PairedBootstrap pbs = new PairedBootstrap(deltaScores, weights);
        	return pbs.avgGain();
        }
	}
} 
