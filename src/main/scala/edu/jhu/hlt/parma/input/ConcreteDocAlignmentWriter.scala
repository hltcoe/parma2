// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 31 August 2013

package edu.jhu.hlt.parma.input

import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.parma.types._
import java.io.File

/**
 * Use this class to read in data from a particular DocAlignmentReader
 * and write it out to disk in Concrete's formats (Discourse and Communications).
 * To read it back again, use ConcreteDocAlignmentReader.
 */
object ConcreteDocAlignmentWriter extends Logging2 {

	def main(args: Array[String]) {
		if(args.length != 2) {
			println("please provide:")
			println("1) a parma.config file")
			println("2) a dataset name [eecb|rf|gv|mtc]")
			return
		}
		ParmaConfig.load(args(0))

		val reader: DocAlignmentReader[_ <: DocAlignment] = args(1).toLowerCase match {
			case "eecb" => EECBDocAlignmentReader
			case "rf" => new RothFrankDocAlignmentReader(true, true)
			case "gv" => throw new RuntimeException("add back this functionality with HITIngester")
			case "mtc" => MTCDocAlignmentReader
			case other => throw new RuntimeException("couldn't lookup reader for dataset: " + other)
		}

		val start = System.currentTimeMillis

		val outfile = ParmaConfig.getFile("data.conversion.alignments")
		if(outfile.exists) {
			warn("output file %s already exists\nplease (re)move it before proceeding"
				.format(outfile.getPath))
			return
		}

		val das: Seq[DocAlignment] = reader.getDocAlignments
		log("read %d document alignments".format(das.size))

		log("writing DocAlignments (as Concrete Discourses) to " + outfile.getPath)
		ConcreteDocAlignmentUtils.serialize(das, outfile)

		val commsWithPredArgs: File = ParmaConfig.getFile("data.conversion.documents", null)
		if(commsWithPredArgs != null) {
			log("writing Concrete Communications with Entities and Situtations to " + commsWithPredArgs.getPath)
			ConcreteWrapper.writeCommunicationsTo(commsWithPredArgs,
				das.flatMap(da => Seq(da.report, da.passage)).toSet[Document].map(_.asInstanceOf[CommunicationDocument].communication).toSeq)
		}
		else log("not saving Concrete Communications with Entities and Situtations because 'data.conversion.documents' is not set")

		log("done saving the %s dataset to %s in %.1f seconds"
			.format(reader.domain, outfile.getPath, (System.currentTimeMillis - start)/1000d))
	}

}


