package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.inference.CanonicalMentionFinder

trait EmbeddingProvider {

	def name: String

	def setup

	/**
	 * vectors returned by this belong to this class
	 * a caller of this function should not mutate a returned DVec
	 */
	def embeddingFor(word: String): Option[DVec]

	/**
	 * every DVec returned by embeddingFor() should be this dimension
	 */
	def dimension: Int
}

object HuangEmbeddingProvider {
	def main(args: Array[String]) {
		if(args.length != 1) {
			println("please provide a parma.config file")
			return
		}
		ParmaConfig.load(args(0))
		val ep = new HuangEmbeddingProvider
		ep.setup
		val words = Seq("president", "Obama", "he", "Washington", "grapefruit", "banana", "stamp", "envelope", "jail", "prison")
		for(w <- words;
			v <- ep.embeddingFor(w))
			println("%s %s".format(w, v.getArray.toSeq.mkString(", ")))
		for(w1 <- words;
			w2 <- words;
			v1 <- ep.embeddingFor(w1);
			v2 <- ep.embeddingFor(w2))
			println("%s %s l2_dist=%.3f dot=%.3f comp_close=%s".format(w1, w2,
				l2_distance(v1, v2),
				VecOps.dot(v1, v2),
				Seq(0.01, 0.1, 0.2, 0.3, 0.4, 0.5).map(e => component_closeness(v1, v2, eps=e)).mkString(", ")))
	}
	def component_closeness(v1: DVec, v2: DVec, eps: Double = 0.1d): Double = {
		var d = 0d
		var i = 0
		val n = v1.dimension
		assert(v2.dimension == n)
		while(i < n) {
			if(math.abs(v1(i) - v2(i)) > eps)
				d += 1d
			i += 1
		}
		n - d
	}
	def l2_distance(v1: DVec, v2: DVec): Double = {
		val d = v1.copy
		VecOps.add(d, v2, -1d)
		d.l2
	}
}

// TODO pick out the prototype that is most likely, or do averaging
class HuangEmbeddingProvider extends EmbeddingProvider with Logging2 {

	val normalize = true

	/**
	 * numVecs is how many prototypes are used for this word (string)
	 * see the paper for more details:
	 * http://www.socher.org/uploads/Main/HuangSocherManning_ACL2012.pdf
	 */
	sealed class HWord(val word: String, val index: Int, val tfIdf: Double, val numVecs: Int) {
		private var embedding: DVec = null
		def setEmbedding(v: DVec) { embedding = v }
		def getEmbedding: DVec = {
			assert(embedding != null)
			embedding
		}
	}

	private var dim = -1
	private var lookup = new collection.mutable.HashMap[String, HWord]

	override def name = "Huang-ACL2012-Embeddings"

	override def setup {
		val vocabFile = ParmaConfig.getFile("features.embeddings.huang.vocab")
		val embFile = ParmaConfig.getFile("features.embeddings.huang.vecs")
		
		log("reading vocab from " + vocabFile.getPath)
		val vocab: IndexedSeq[HWord] = io.Source.fromFile(vocabFile).getLines.zipWithIndex.map(li => {
			val (line, index) = li
			val ar = line.split("\t")
			assert(ar.length == 3)
			new HWord(ar(0), index, ar(1).toDouble, ar(2).toInt)
		}).toIndexedSeq

		log("reading embeddings from " + embFile.getPath)
		val t = Profiler.getTime {
			val vecs = io.Source.fromFile(embFile).getLines.map(embeddingLine2Vec).toIndexedSeq
			for((hw, v) <- vocab.zip(vecs)) {
				hw.setEmbedding(v)
				lookup.put(hw.word, hw)
			}
		}
		log("done, took %.1f seconds".format(t))
	}

	private def embeddingLine2Vec(line: String): DVec = {
		val st = new java.util.StringTokenizer(line, ",")
		if(this.dim < 0)
			this.dim = st.countTokens
		val vec = DVec.zero(this.dim)
		var i = 0
		while(st.hasMoreTokens) {
			vec(i) = st.nextToken.toDouble
			i += 1
		}
		assert(i == this.dim)
		if(normalize)
			vec *= 1d / vec.l2
		vec
	}
	
	override def embeddingFor(word: String): Option[DVec] = {
		lookup.get(word) match {
			case Some(v) => Some(v.getEmbedding)
			case None => lookup.get(word.toLowerCase).map(_.getEmbedding)
		}
	}

	override def dimension: Int = {
		assert(this.dim > 0)
		this.dim
	}
}

class HuangEmbeddingFeature extends EmbeddingFeature(new HuangEmbeddingProvider)

class EmbeddingFeature(val provider: EmbeddingProvider) extends AlignmentSimilarity {

	// TODO random projections to get a more reasonable size (I can't afford to learn hundreds of weights for this feature)
	val includeEmbeddingDiff = false

	val useLemma = true
	val useBinaryCloseness = false; val eps = 0.1	// 1 if abs(rv(i) - pv(i)) < eps
	val scale = 1d			// bigger value => less regularization on this feature, only applys to index-level features
	lazy val euclideanDistIdx = provider.dimension
	lazy val dotIdx           = provider.dimension + 1
	lazy val oneVecMissingIdx = provider.dimension + 2
	lazy val twoVecMissingIdx = provider.dimension + 3
	val stats = new CacheStats(Some("words-with-embeddings-available"), interval=5000)

	override def name: String = "Embedding#" + provider.name
	override def featureName(idx: Int) = {
		require(idx <= twoVecMissingIdx)
		"Embedding#%s@%d".format(provider.name, idx)
	}

	override def setup(calibrateOn: java.util.Collection[DocAlignment]) {
		provider.setup
	}

	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {
		val (reportMention, passageMention) = CanonicalMentionFinder.canonicalMentions(a, report, passage)
		val rh = report.getHeadToken(reportMention)
		val ph = passage.getHeadToken(passageMention)
		val (rs, ps) =
			if(useLemma) (rh.getLemma, ph.getLemma)
			else (rh.getWord, ph.getWord)
		val rvo = provider.embeddingFor(rs.toLowerCase)
		val pvo = provider.embeddingFor(ps.toLowerCase)
		(rvo, pvo) match {
			case (Some(rv), Some(pv)) =>
				// take the difference of the embeddings, treat this as a set of features
				var i = 0
				var dist = 0d
				var dot = 0d
				while(i < rv.dimension) {	// slow, but i don't think anything can be done about it
					val d = rv(i) - pv(i)
					dot += rv(i) * pv(i)
					dist += d*d
					if(includeEmbeddingDiff) {
						if(useBinaryCloseness)
							sv.add(i, if(math.abs(d) < eps) scale else 0d)
						else
							sv.add(i, scale * d)
					}
					i += 1
				}
				sv.add(euclideanDistIdx, math.sqrt(dist))
				sv.add(dotIdx, dot)
				stats.hit
			case (Some(_), None) | (None, Some(_)) =>
				sv.add(oneVecMissingIdx, 1d)
				stats.miss
			case (None, None) =>
				sv.add(twoVecMissingIdx, 1d)
				stats.miss
		}
	}
}

