package edu.jhu.hlt.parma.inference.temporal

import edu.jhu.hlt.parma.types._

/*
$ pwd
  /home/hltcoe/twolfe/fall2013/ts-from-ben/TimeSieve/time-sieve/src/main/resources
$ grep -i tlink tempeval3-timebankonly-full.xml | key-values relation | sort | uniq -c
    679 relation="AFTER"
   1194 relation="BEFORE"
     45 relation="BEGINS"
     48 relation="BEGUN_BY"
    115 relation="ENDED_BY"
     60 relation="ENDS"
     32 relation="IAFTER"
     26 relation="IBEFORE"
    472 relation="INCLUDES"
   1115 relation="IS_INCLUDED"
   1335 relation="SIMULTANEOUS"
*/

/**
 * missing some that are not available in my data now
 * http://timeml.org/site/publications/timeMLdocs/annguide_1.2.1.pdf
 */
sealed abstract class TimeMLRelation(val firstPred: Predicate, val secondPred: Predicate) extends Serializable {

	/**
	 * BEFORE.inverse == AFTER
	 * SIMULTANEOUS.inverse = SIMULTANEOUS
	 * etc, switch first and second preds to make it still true
	 */
	def inverse: TimeMLRelation

	/** should be in (0, 1] */
	def confidence: Double
}

case class Before(p1: Predicate, p2: Predicate, val confidence: Double = 1d) extends TimeMLRelation(p1, p2) {
	override def inverse = After(p2, p1, confidence)
}
case class After(p1: Predicate, p2: Predicate, val confidence: Double = 1d) extends TimeMLRelation(p1, p2) {
	override def inverse = Before(p2, p1, confidence)
}

case class BegunBy(p1: Predicate, p2: Predicate, val confidence: Double = 1d) extends TimeMLRelation(p1, p2) {
	override def inverse = Begins(p2, p1, confidence)
}
case class Begins(p1: Predicate, p2: Predicate, val confidence: Double = 1d) extends TimeMLRelation(p1, p2) {
	override def inverse = BegunBy(p2, p1, confidence)
}

case class EndedBy(p1: Predicate, p2: Predicate, val confidence: Double = 1d) extends TimeMLRelation(p1, p2) {
	override def inverse = Ends(p2, p1, confidence)
}
case class Ends(p1: Predicate, p2: Predicate, val confidence: Double = 1d) extends TimeMLRelation(p1, p2) {
	override def inverse = EndedBy(p2, p1, confidence)
}

case class Includes(p1: Predicate, p2: Predicate, val confidence: Double = 1d) extends TimeMLRelation(p1, p2) {
	override def inverse = IsIncluded(p2, p1, confidence)
}
case class IsIncluded(p1: Predicate, p2: Predicate, val confidence: Double = 1d) extends TimeMLRelation(p1, p2) {
	override def inverse = Includes(p2, p1, confidence)
}

case class ImmediatelyBefore(p1: Predicate, p2: Predicate, val confidence: Double = 1d) extends TimeMLRelation(p1, p2) {
	override def inverse = ImmediatelyAfter(p2, p1, confidence)
}
case class ImmediatelyAfter(p1: Predicate, p2: Predicate, val confidence: Double = 1d) extends TimeMLRelation(p1, p2) {
	override def inverse = ImmediatelyBefore(p2, p1, confidence)
}

case class Simultaneous(p1: Predicate, p2: Predicate, val confidence: Double = 1d) extends TimeMLRelation(p1, p2) {
	override def inverse = Simultaneous(p2, p1, confidence)
}


