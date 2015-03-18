package edu.jhu.hlt.parma.inference.maxmargin

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.inference.DocMetaAligner
import java.io.File

object CPGlobalAlignmentTest extends Logging2 {

	import edu.jhu.hlt.parma.inference.maxmargin.TestData
	import edu.jhu.hlt.parma.util.ParamRefRangeImplicits._

	def main(args: Array[String]) {
		if(args.length != 1) {
			log("please give a parma.config file")
			return
		}
		ParmaConfig.load(args(0))

		println("classpath = " + System.getProperty("java.class.path"))

		lazy val das = TestData.das.sortBy(da => DocMetaAligner.allPossibleAlignments(da.context).size)

		test0

		this.redirectLogTo(new File(TestData.logDir, "cpgTest1-main.log"))

		for(da <- das.take(250))
			test3(da)

		for(da <- das.take(25))
			test2(da)

		log("attempting test1 with NO ferts")
		for(da <- das.take(50))
			test1(da, 0)
		log("attempting test1 with fert=1")
		for(da <- das.take(25))
			test1(da, 1)
		log("attempting test1 with fert=2")
		for(da <- das.take(100))
			test1(da, 2)

		log("congrats! you passed all the tests")
	}

	def test0 {
		log("starting test0")
		// if fert1 >> best score => nothing is aligned
		// if fert1 < best score && fert2 >> best score => 0-1 alignments
		// if fert1 == fert2 == 0 => everything with score>0 should be aligned

		import edu.jhu.hlt.concrete.io.ProtocolBufferReader
		import edu.jhu.hlt.concrete.Concrete.Communication
		val commFile = "data/eecb/eecb-docs-concrete.pb"
		val comm = new ProtocolBufferReader(commFile, classOf[Communication]).next
		log("comm id = " + comm.getGuid.getCommunicationId)
		assert(comm.getGuid.getCommunicationId == "1_10.eecb")

		// make some preds/args manually and add them to a doc builder
		val doc = new RichConcreteDocBuilder(comm)
		val p1 = Predicate(MentionBuilder.from(0, 0, 1, 0))
		val p2 = Predicate(MentionBuilder.from(0, 3, 4, 3))
		val p3 = Predicate(MentionBuilder.from(0, 5, 6, 5))
		doc.addPredicate(p1)
		doc.addPredicate(p2)
		doc.addPredicate(p3)

		// try to align this with itself
		val c = Context(doc, doc);

		val theta = DVec.rep(0d, 100)
		def assertNumAlignments(a: CPGlobalAlignment, n: Int) {
			val da = a.decode(theta)
			val h = da.sureAlignments.size
			require(h == n, "#alignments=%d expected %d".format(h, n))
		}

		val fert1FeatNames: IndexedSeq[String] = IndexedSeq("fert1-c", "fert1-p", "fert1-a") ++ (1 to 7).map(_ => "idk")
		val fert2FeatNames: IndexedSeq[String] = IndexedSeq("fert2-c", "fert2-p", "fert2-a") ++ (1 to 7).map(_ => "idk2")
		
		{	// up to fert1
			log("testing fert1")
			val fert1 = new FixedParamRefRange(5, fert1FeatNames, theta) with IsActive
			val fert2 = new FixedParamRefRange(15, fert2FeatNames, theta) with IsNotActive

			val a = new CPGlobalAlignment(10)
			a.initData(c, (a: Alignment, c: Context) => {
				val pa = a.asInstanceOf[PredicateAlignment]
				val s = if(pa.reportPred == pa.passagePred) 1d else -1e-4
				SVec((1, s))
			})
			a.addFertilityFactors(fert1, fert2, false, false)

			theta.zeroOut
			theta(1) = 1d

			theta(fert1.index(0)) = -10d	// need (0) because we're using the centroid feature
			assertNumAlignments(a, 0)

			theta(fert1.index(0)) = 0d
			assertNumAlignments(a, 3)

			// if fert1 is positive, we should still see a diag alignment
			// this is because by activating anything in a row/col, you already
			// set the fert1 variable and get the benefit (-1 cost)
			// any others you set will lower the score by 1e-4
			theta(fert1.index(0)) = 1d
			assertNumAlignments(a, 3)
		}

		// TODO generate a bunch of cases using fert1 vs fert2
		// in the case of fert2, make sure that penalty(fert2) == 0
		// and make sure that you get the same alignments from both setups

		{	// up to fert2
			log("testing fert2")
			val fert1 = new FixedParamRefRange(5, fert1FeatNames, theta) with IsActive
			val fert2 = new FixedParamRefRange(15, fert2FeatNames, theta) with IsActive

			val a = new CPGlobalAlignment(10)
			a.initData(c, (a: Alignment, c: Context) => {
				val pa = a.asInstanceOf[PredicateAlignment]
				val s = if(pa.reportPred == pa.passagePred) 1d else 1e-3
				SVec((1, s))
			})

			theta.zeroOut
			theta(1) = 1d
			a.addFertilityFactors(fert1, fert2, false, false)
			a.updateSimilarities(theta)

			println("alignment = " + a.debugState)
			// #pred = 9
			// -> 3x3 grid (this is an alignment of a doc to itself)

			theta(fert1.index(0)) = -4d
			theta(fert2.index(0)) = -10d
			assertNumAlignments(a, 0)

			theta(fert1.index(0)) = 0d
			theta(fert2.index(0)) = 0d	
			assertNumAlignments(a, 9)

			theta(fert1.index(0)) = -0.1d
			theta(fert2.index(0)) = -0.1d	
			assertNumAlignments(a, 3)
			
			theta(fert1.index(0)) = -(1d + 1e-5)
			theta(fert2.index(0)) = 0d
			assertNumAlignments(a, 0)

			theta(fert1.index(0)) = 0d
			theta(fert2.index(0)) = -1.1d
			assertNumAlignments(a, 3)

			theta(fert1.index(0)) = 0d
			theta(fert2.index(0)) = 1d
			assertNumAlignments(a, 9)

			// this test fails due to the one-sidedness of the constraint
			// it can get the +10 from fert2 without turning on the individual alignments
			//theta(fert1.index(0)) = -(1d + 1e-5)
			//theta(fert2.index(0)) = 10d
			//assertNumAlignments(a, 9)

			// /2 because you will be hit with a row and col fert1 factor
			theta(fert1.index(0)) = -0.5d / 2 - 1e-5
			theta(fert2.index(0)) = -1d
			assertNumAlignments(a, 3)
		}
	}

	def initializedAlignment(da: DocAlignment, dim: Int, adds: Int = 10, cacheSize: Int = 25): CPGlobalAlignment = {
		val fnPen = 1d
		val a = new CPGlobalAlignment(cacheSize)
		def featureFunc(a: Alignment, c: Context): SVec = {
			val sv = new SVec
			for(i <- 1 to adds) {
				val idx = TestData.rand.nextInt(dim)
				val value = TestData.rand.nextGaussian
				sv.add(idx, value)
			}
			sv
		}
		a.initData(featureFunc, da, fnPen)
		a
	}

	def test2(da: DocAlignment) {

		var theta = DVec.rep(0d, 150)
		val a = initializedAlignment(da, theta.dimension)

		val fert1FeatNames: IndexedSeq[String] = IndexedSeq("fert1-c", "fert1-p", "fert1-a") ++ (1 to 7).map(_ => "idk")
		val fert2FeatNames: IndexedSeq[String] = IndexedSeq("fert2-c", "fert2-p", "fert2-a") ++ (1 to 7).map(_ => "idk2")

		val fert1 = new FixedParamRefRange(5, fert1FeatNames, theta) with IsActive
		val fert2 = new FixedParamRefRange(15, fert2FeatNames, theta) with IsActive
		//val fert1 = new FixedParamRef("fert1", 140, theta) with IsActive
		//val fert2 = new FixedParamRef("fert2", 141, theta) with IsActive
		a.addFertilityFactors(fert1, fert2, false, false)

		// sice all the weights are 0, oracle = argmax_z score(z) + loss(z)
		// can only maximize loss (score is a constant function at 0)
		// so it will choose an alignment that differs in every component (max hamming loss)
		var cons = new Constraint(theta.dimension)
		a.addMostViolatedConstraint(theta, cons)
		
		// differ in every alignment
		log("initial mv constraint's loss: " + cons.getLoss)
		//assert(cons.getLoss == DocMetaAligner.allPossibleAlignments(da.context).size)
	}

	/**
	 * hinge(theta, z) = loss(z) + score(theta, z)
	 * oracle(theta) = argmax_z hinge(z, theta)
	 * hinge(theta, oracle(theta)) >= hinge(theta, z) \forall z
	 *
	 * in this case, i chose another z by solving a different oracle
	 * also, z is encoded in a Constraint
	 */
	def test3(da: DocAlignment) {

/*
		log("[test3] starting #predAlignments=%d #argAlignments=%d"
			.format(DocMetaAligner.allPossiblePredAlignments(da.context).size,
				DocMetaAligner.allPossibleArgCorefAlignments(da.context).size))

		val dim = 150
		val t1 = DVec.gaussian(dim, 1d, TestData.rand)
		val t2 = DVec.gaussian(dim, 1d, TestData.rand)
		assert(t1 != t2)
	
		val a = initializedAlignment(da, dim)
		// no fertility factors, because they capture a reference to theta, which is terrible

		val z1 = new Constraint(dim)
		val z2 = new Constraint(dim)

		a.addMostViolatedConstraint(t1, z1)	// z1 = oracle(t1)
		//log("[test3] after computing z1:")
		//log(a.toString)

		a.addMostViolatedConstraint(t2, z2)	// z2 = oracle(t2)
		//log("[test3] after computing z2:")
		//log(a.toString)

		log("z1.hinge(t1)=%.8g z2.hinge(t1)=%.8g".format(z1.hinge(t1), z2.hinge(t1)))
		log("z2.hinge(t2)=%.8g z1.hinge(t2)=%.8g".format(z2.hinge(t2), z1.hinge(t2)))
		assert( z1.hinge(t1) >= z2.hinge(t1) )
		assert( z2.hinge(t2) >= z1.hinge(t2) )
*/

		// WHAT I'M TESTING:
		// z1.hinge(t1)                                    >=  z2.hinge(t1)
		// max(0, loss(z1) + t1 * { f(z1) - f(z1_lab) } )  >=  max(0, loss(z2) + t1 * { f(z2) - f(z2_lab) } )

		// WHAT I SHOULD BE TESTING
		// max(0, loss(z1) + t1 * f(z1))  >=  max(0, loss(z2) + t1 * f(z2))

		// because z1_lab and z2_lab are not the same, this is not a valid test
		// i don't currently have a way to test this because i only store f(z') - f(z)


		/*
		val h11 = z1.hinge(t1)
		val h12 = z1.hinge(t2)
		val h22 = z2.hinge(t2)
		val h21 = z2.hinge(t1)
		log("z1=" + z1)
		log("z2=" + z2)
		log("h11=%.8f h12=%.8f".format(h11, h12))
		log("h22=%.8f h21=%.8f".format(h22, h21))
		assert(h11 <= h21)	// hinge is a non-negative value in terms of loss
		assert(h22 <= h12)
		*/
	}

	/**
	 * cache should always find the same most violated constraint as cplex
	 *
	 * @param fert=0 means no fertility, fert=1 means 0-1 penalties, fert=2 means quadratic model
	 * 
	 * currently, this test uses the initData function that takes a DocAlignment
	 * (which means it has labels), so this is adding loss loss function.
	 * presumably this also means that the loss function factors are being added,
	 * and the most violated constraint code is incorporating CPHammingFactors
	 */
	def test1(da: DocAlignment, fert: Int) {
		require(fert <= 2)
		log("starting test1 " + da.id)
		val theta = DVec.zero(150)
		val a = initializedAlignment(da, theta.dimension)

		val fert1 = new FixedParamRef("fert1", 140, theta) with Activatable
		val fert2 = new FixedParamRef("fert2", 141, theta) with Activatable
		if(fert == 0) {
			fert1.deactivate
			fert2.deactivate
		}
		else if(fert == 1)
			fert2.deactivate
	
		a.addFertilityFactors(fert1, fert2, false, false)

		// make sure fertility penalties are >0
		theta(fert1.index) = -1e-5
		theta(fert2.index) = -1e-5

		a.redirectLogTo(new File(TestData.logDir, "cpgTest1.log"))
		for(i <- 1 to 75) {

			log("iteration " + i)

			// compute CPLEX most violated constraint (this adds it to the cache)
			val cplexCons = new Constraint(theta.dimension)
			a.addMostViolatedConstraint(theta, cplexCons)

			// compute most violated in cache => should be the same as CPLEX
			val cacheCons = new Constraint(theta.dimension)
			a.addMostViolatedConstraintCached(theta, cacheCons)

			log("[test1] cplex=%s cache=%s".format(cplexCons, cacheCons))
			log("[test1] cplex.hinge=%.8g cache.hinge=%.8g".format(cplexCons.hinge(theta), cacheCons.hinge(theta)))
			try {
				// there are two places hinge(cons, theta) can come from
				// 1) CPG.addMostViolatedConstraint
				// 2) mvCache.maxBy(...)
				//
				// 2 is pretty hard to argue with
				// (and it seems that it is doing what it should be:
				//  it finds the highest hinge(cons, theta) in the cache)

				// so maybe cplex is not solving for the correct thing?
				// it seems to work fine for fert<2, so maybe look at Fert2 class...
				// 

				assert( math.abs(cplexCons.hinge(theta) - cacheCons.hinge(theta)) < 1e-6 )
				/*
				assert(cplexCons == cacheCons)
				assert(cplexCons.getSig == cacheCons.getSig)
				assert(cplexCons.getLoss == cacheCons.getLoss)
				assert(cplexCons.getDeltaFeatures == cacheCons.getDeltaFeatures)
				*/
			} catch {
				case t: Throwable =>
					
					/* this doesn't seem to be working
					val repl = new tools.nsc.interpreter.ILoop
					repl.settings = new tools.nsc.Settings
					repl.in = tools.nsc.interpreter.SimpleReader()
					repl.createInterpreter()
					repl.intp.bind("cplexCons", "Constraint", cplexCons)
					repl.intp.bind("cacheCons", "Constraint", cacheCons)
					repl.loop
					repl.closeInterpreter
					*/

					log("cache.hinge %s %.8g".format(cacheCons, cacheCons.hinge(theta)))
					log("cplex.hinge %s %.8g".format(cplexCons, cplexCons.hinge(theta)))

					//for(((cplex, cache), idx) <- (cplexCons.deltas.zip(cacheCons.deltas)).zipWithIndex)
					//	log("%d eq? %s".format(idx, cplex == cache))
					log("contents of this CPGlobalAlignment's cache:")
					for(((df, loss, sig), idx) <- a.mvCache.zipWithIndex) {
						val c = new Constraint(theta.dimension)
						c.addAlignment(df, loss, sig)
						val h = c.hinge(theta)
						log("%d: sig=%d loss=%.3f df.l2=%.3f df.l1=%.3f hinge=%.8g"
							.format(idx, sig, loss, df.l2, df.l1, h))
					}

					//log(t.getMessage)
					//t.printStackTrace(new PrintStream(getLogger.getOutputStream))
					throw t
			}
			theta += DVec.gaussian(theta.dimension, 1d, TestData.rand)

			// make sure fertility penalties are >0
			// TODO see if you can pass this test without this step (i think we should be able to)
			theta(fert1.index) = -math.pow(TestData.rand.nextGaussian, 2d)
			theta(fert1.index) = theta(fert1.index) - math.pow(TestData.rand.nextGaussian, 2d)
		}
	}
}


