// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.concrete.Concrete._
import edu.jhu.hlt.concrete.io.{ ProtocolBufferReader, ProtocolBufferWriter }
import java.io._
import java.util.zip._
import collection.mutable.ArrayBuffer
import collection.JavaConversions._

object ConcreteWrapper extends Logging2 {
	
	object TadsFolder extends Logging2 {

		import io.Source
		import java.math.BigInteger
		import java.util.UUID
		import edu.jhu.hlt.concrete.Concrete



		// TODO this is broken, problem with signs on longs
		implicit def uuid_conc2java(in: Concrete.UUID): java.util.UUID = new java.util.UUID(in.getHigh, in.getLow)
	
		/**
		 * url is the URL of the website (not supposed to have "/export/common..." in it)
		 * uuid is broken...
		 * location is the Communications file this Communication is in
		 */
		case class CommunicationLocation(val url: String, val uuid: UUID, val location: File)

		def stripJunk(guid: String): String = {
			guid.trim
				.replaceFirst("""https?[:-]//""", "")
				.replaceFirst("""/export/common/SCALE13/Text/webcrawls-txt/global-voices-gigaword/external-links/""", "")
				.replaceFirst("""/export/common/SCALE13/Text/webcrawls-txt/global-voices-gigaword/""", "")
				.replaceAll(""".xml$""", "")
		}

		/**
		 * looks up a Communications UUID in the URL index files
		 */
		def findCommunicationsFaster(commDir: File, urls: Set[String]): Seq[Communication] = {
			val commsByURL: Map[String, CommunicationLocation] = communicationsByURL(commDir)
				.mapValues(loc => CommunicationLocation(stripJunk(loc.url), loc.uuid, loc.location))
			val locations = urls.map(commsByURL.apply)
			var nSeen = 0
			val communications = new ArrayBuffer[Communication]
			for((f, locs) <- locations.groupBy(_.location)) {

				//val needToFind = new collection.mutable.HashSet[UUID]
				//needToFind ++= locs.map(_.uuid)
				val needToFind = new collection.mutable.HashSet[String]
				needToFind ++= locs.map(_.url)
				println("opening %s, need to find: %s".format(f.getPath, needToFind.mkString(", ")))

				var done = false
				var localOffset = 0
				val pbr = new ProtocolBufferReader(f.getPath, classOf[Communication])
				while(pbr.hasNext && !done) {
					try {
						val c = pbr.next
						nSeen += 1
						val docId = stripJunk(c.getGuid.getCommunicationId)
						//println("docId = \"%s\"".format(docId))
						if(needToFind.contains(docId)) {
							communications += c
							needToFind -= docId
							done = (needToFind.size == 0)
							log("found %d of %d Communications, have looked through %d".format(communications.size, urls.size, nSeen))
						}
						if(nSeen % 1000 == 0)
							log("found %d of %d Communications, have looked through %d".format(communications.size, urls.size, nSeen))
						localOffset += 1
					} catch {
						case e: Exception =>
							e.printStackTrace
							warn("this was the document %d (0-indexed) in %s".format(localOffset, f.getPath))
					}
				}
				println("\twent through %d documents in %s".format(localOffset, f.getPath))
				pbr.close
			}
			communications.toSeq
		}

		/**
		 * returns all CommunicationLocations found in ID_MAP_PROTO_* files
		 * indexed by URL
		 */
		def communicationsByURL(commDir: File): Map[String, CommunicationLocation] = {
			val indexPrefix = "ID_MAP_"// "travis-ids-"	
			def line2pair(line: String): (String, UUID) = {
				val ar = line.split("\t")
				require(ar.length == 3)
				val url = ar(0)
				val uuid_hi = new BigInteger(ar(1)).longValue
				val uuid_lo = new BigInteger(ar(2)).longValue
				(stripJunk(url), new UUID(uuid_hi, uuid_lo))	// TODO wrong
			}
			def locationsIn(f: File): Seq[CommunicationLocation] = {
				Source.fromFile(f).getLines.flatMap(line => {
					try {
						val (url, uuid) = line2pair(line)
						Some(CommunicationLocation(url, uuid, new File(f.getPath.replace(indexPrefix, ""))))
					} catch {
						case e: Exception => None
					}
				}).toSeq
			}
			val locs = commDir.list.filter(_.matches(indexPrefix + """PROTO_\d+""")).flatMap(fn => locationsIn(new File(commDir, fn)))
			val index = locs.groupBy(_.url)
			if(index.size != locs.size) {
				warn("overlap = " + index.iterator.filter(_._2.size > 1).take(10).mkString(", "))
				warn("index.size=%d locs.size=%d".format(index.size, locs.size))
			}
			index.mapValues(_.apply(0))
		}



		/**
		 * commDir should be a directory with the same structure as:
		 * /export/common/SCALE13/Text/tturpen/corpus/GlobalVoices/concrete/v01
		 *
		 * check a communications Guid.CommunicationId against the provided set
		 * (not really fast, still does linear scan)
		 */
		def findCommunicationsSlow(commDir: File, communicationGuids: Set[String]): Seq[Communication] = {
			val seen = new collection.mutable.HashSet[String]
			val comms = new ArrayBuffer[Communication]
			var nSeen = 0
			var done = false
			val files = commDir.list.filter(_.matches("""PROTO_\d+""")).map(new File(commDir, _)).toIterator
			while(!done) {
				if(files.hasNext) {
					var localOffset = 0
					val f = files.next
					val pbr = new ProtocolBufferReader(f.getPath, classOf[Communication])
					while(pbr.hasNext && !done) {
						try {
							val c = pbr.next
							nSeen += 1
							val docId = c.getGuid.getCommunicationId
							if(communicationGuids.contains(docId)) {
								if(seen.add(docId)) comms += c
								else warn("found a duplicate: \"%s\" (keeping the first instance)".format(docId))
								done = (seen.size == communicationGuids.size)
								log("found %d of %d Communications, have looked through %d".format(seen.size, communicationGuids.size, nSeen))
							}
							if(nSeen % 1000 == 0)
								log("found %d of %d Communications, have looked through %d".format(seen.size, communicationGuids.size, nSeen))
							localOffset += 1
						} catch {
							case e: Exception =>
								e.printStackTrace
								warn("this was the document %d (0-indexed) in %s".format(localOffset, f.getPath))
						}
					}
					pbr.close
				}
				else done = true
			}
			if(seen.size != communicationGuids.size) {
				warn("set out to find %d Communications and found %d".format(communicationGuids.size, seen.size))
				warn("didn't find " + (communicationGuids -- seen).map("\"" + _ + "\"").mkString(", "))
			}
			comms.toSeq
		}
	}

	def writeCommunicationsTo(f: File, comms: Seq[Communication]) {
		log("writing %d Communications to %s".format(comms.size, f.getPath))
		val pbw = new ProtocolBufferWriter(f.getPath)
		comms.foreach(pbw.write)
		pbw.close
	}
	
	def getCommunicationsFrom(f: File): IndexedSeq[Communication] =
		getCommunicationsFrom(f, (c: Communication) => true)

	/**
	 * only keeps in memory Communications for which keep returns true
	 * can be used with very large protobuf files
	 */
	def getCommunicationsFrom(f: File, keep: Communication => Boolean): IndexedSeq[Communication] = {
		val pbr = getCommReader(f)
		val buf = new ArrayBuffer[Communication]
		while(pbr.hasNext) {
			val comm = pbr.next
			if(keep(comm))
				buf += comm
		}
		pbr.close
		buf.toIndexedSeq
	}

	def getDocumentsFrom(f: File): IndexedSeq[Document] =
		getDocumentsFrom(f, (d: Document) => true)
	
	/**
	 * only keeps in memory Documents for which keep returns true
	 * can be used with very large protobuf files
	 */
	def getDocumentsFrom(f: File, keep: Document => Boolean): IndexedSeq[Document] = {
		val pbr = getCommReader(f)
		val buf = new ArrayBuffer[Document]
		while(pbr.hasNext) {
			val doc = new RichConcreteDoc(pbr.next)
			if(keep(doc))
				buf += doc
		}
		pbr.close
		buf.toIndexedSeq
	}

	def getCommReader(f: File) = {
		if(!f.exists || !f.isFile)
			throw new RuntimeException("please provide a valid file: " + f.getPath)
		val fis = new FileInputStream(f)
		val is =
			if(f.getName.toLowerCase.endsWith("gz"))
				new GZIPInputStream(fis)
			else fis
		log("[ConcreteWrapper getCommReader] reading from " + f.getPath)
		log(Describe.memoryUsage(true))
		new ProtocolBufferReader[Communication](is, classOf[Communication])
	}

}

