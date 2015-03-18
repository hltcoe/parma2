// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.diagnostics

import edu.jhu.hlt.parma.inference._
import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.experiments._
import edu.jhu.hlt.parma.util._
import scala.collection.JavaConversions._
import java.util.logging._
import java.io.File

object CanonicalMentionDiagnostics {

    val log = Logger.getLogger(this.getClass.getName)
    val OUTPUT = "diagnostics.canonical.mention.file"

    def printCanonicalMentions(corpus: DocAlignmentCorpus[DocAlignment]) {

        val outfile = ParmaConfig.getFile(OUTPUT)
        if(outfile == null || !outfile.exists) {
            log.info("[printCanonicalMentions] skipping because no file was specified in parma.config with " + OUTPUT)
            return
        }
        Profiler.startTask("diagnotics:canonical_mentions")
        log.info("[printCanonicalMentions] writing canonical mentions to " + outfile)
        val writer = FileUtils.getWriter(outfile)

        // function to print out canonical mentions
        val pcm = (ac: ArgumentCoref, doc: Document) => {
            val canonical = CanonicalMentionFinder.canonicalMention(ac, doc)
            writer.write("coref chain =\n")
            ac.foreach(m => {
				val score = CanonicalMentionFinder.scoreCanonicalMention(doc, m.location)
				val s = Describe.argument(m, doc)
                val pos = doc.getMentionTokens(m).map(_.getPosTag).mkString(" ")
                val ner = doc.getMentionTokens(m).map(_.getNerTag).mkString(" ")
                writer.write("\t%s \t POS = %s \t NER = %s \t score = %.3g\n".format(s, pos, ner, score))
            })
            writer.write("canonical = %s\n".format(Describe.argument(canonical, doc)))
            writer.newLine
        }

        // only go over train mentions for now
        corpus.trainAlignments.foreach(da => {
            da.report.corefs.filter(_.size > 1).foreach(ac => pcm(ac, da.report))
            da.passage.corefs.filter(_.size > 1).foreach(ac => pcm(ac, da.passage))
        })

		// for each postitive alignment, just write out the headwords next to each other
		writer.newLine
		writer.newLine
		writer.write("====================== just head words for (all) true alignments ======================\n")
		for(da <- corpus.trainAlignments ++ corpus.testAlignments) {
			writer.write("alignment %s\n".format(da.id))
			for(a <- da.possibleAlignments)
				writer.write(Describe.justHeadWord(a, da.report, da.passage) + "\n")
		}

        writer.close
        Profiler.endTask("diagnotics:canonical_mentions")
    }

}

