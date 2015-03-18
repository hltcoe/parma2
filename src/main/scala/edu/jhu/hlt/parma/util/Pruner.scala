package edu.jhu.hlt.parma.util

import edu.jhu.hlt.parma.inference.maxmargin._
import util.Random

trait Pruner[T,C] {
    def prune(things: Seq[T], context: C): Seq[T]
}

class UniformCutoffPruner[T,C](val cutoff: Int, val rand: Random) extends Pruner[T,C] {
    import edu.jhu.hlt.parma.util.Reservoir.Sample
    def this(cutoff: Int) = this(cutoff, new Random(9001))
    override def prune(things: Seq[T], context: C): Seq[T] = {
        if(things.size > cutoff)
            things.reservoir(cutoff)(rand)
        else things
    }
}

/**
 * pick off the big ones first
 */
class DefaultFert2Pruner(val howMany: Int, val rand: Random) extends Pruner[CPFactor, CPGlobalAlignment] {
    def this(cutoff: Int) = this(cutoff, new Random(9001))
	override def prune(fert2s: Seq[CPFactor], cpg: CPGlobalAlignment): Seq[CPFactor] = {
		fert2s
			.map(x => (x, rand.nextGaussian * 0.5))	// fuzz so that ties are handled well
			.sortBy(_ match {
				case (m: CPFertilityFactor2, r: Double) =>
					m.alignments.size + r + 10d
				case (s: CPFertilityFactor2Sum, r: Double) =>
					s.alignments.size + r + 10d
				case _ =>
					0d	// pass through anything thats not a fert2
			})
			.take(howMany)
			.map(_._1)
	}
}

