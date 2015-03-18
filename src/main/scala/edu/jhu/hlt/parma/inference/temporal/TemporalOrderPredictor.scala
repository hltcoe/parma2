package edu.jhu.hlt.parma.inference.temporal

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import timesieve._
import timesieve.tlink._
import collection.mutable.ArrayBuffer
import collection.JavaConversions._
import scala.util.Random
import java.io.File

/**
 * predicts the temporal ordering of events mentioned in a document
 */
trait TemporalOrderPredictor {
	/**
	 * should return 1 if p2 certainly came after p1
	 * -1 if p1 certainly came after p2
	 * and 0 if you have no idea or there is no clear relation
	 */
	def after(p1: Predicate, p2: Predicate): Double
}

/**
 * makes up a temporal ordering over all the predicates in both documents
 * adds a factor for every 4 predicates (2 predicate-alignments)
 * we will flip the correct label with prob = 1 / (1 + exp(information))
 *   so if information=0 the factors see random labels, if information=infty the factors see the truth
 *   always says that this ordering label has confidence 1
 */
class SyntheticTemporalRelationPredictor(val information: Double, val preds: IndexedSeq[Predicate], val rand: Random)
		extends TemporalRelationPredictor with Logging2 {

	require(information >= 0d)

	val perm = rand.shuffle(1 to preds.size)
	val permMap: Map[Predicate, Int] = preds.zip(perm).toMap
	val pFlip = 1d / (1d + math.exp(information))

	private var flipped = 0
	private var shown = 0

	override def toString: String = "(SyntheticTemporalOrderPredictor info=%.1f flipped %d of %d)"
		.format(information, flipped, shown)

	override def relationship(p1: Predicate, p2: Predicate): Option[TimeMLRelation] =
		Some(if(after(p1, p2) > 0d) After(p1, p2) else Before(p1, p2))

	override def after(p1: Predicate, p2: Predicate): Double = {
		shown += 1
		val lab = if(permMap(p1) < permMap(p2)) 1d else -1d
		if(rand.nextDouble < pFlip) {
			flipped += 1
			-lab
		}
		else lab
	}
}


