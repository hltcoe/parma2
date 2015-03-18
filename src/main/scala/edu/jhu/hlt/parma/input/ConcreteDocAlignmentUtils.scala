// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.input

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.inference.{ CanonicalMentionFinder, DocMetaAligner }
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.concrete.Concrete._
import edu.jhu.hlt.concrete.util.IdUtil
import collection.mutable.ArrayBuffer
import collection.JavaConversions._
import java.util.Calendar
import java.io._

object ConcreteDocAlignmentUtils extends Logging2 {

	val verbose = false

	/**
	 * read a bunch of doc alignments from a file.
	 * file must have been written by serialize()
	 * NOTE: parma-proprietary, does not play nice with Concrete utils
	 */
	def deserialize(f: File): Iterator[DocAlignment] = {
		val iter = new ConcreteDocAlignmentIterator(f)
		log("[ConcreteDocAlignmentUtils deserialize] about to read %d DocAlignments as Discourses and Communications from %s"
			.format(iter.size, iter.source.getPath))
		iter
	}

	/**
	 * save a bunch of doc alignments to a file
	 * NOTE: parma-proprietary, does not play nice with Concrete utils
	 */
	def serialize(das: Seq[DocAlignment], f: File) {
		log("[ConcreteDocAlignmentUtils serialize] about to write %d DocAlignments as Discourses and Communications to %s"
			.format(das.size, f.getPath))
		val dos = new DataOutputStream(new FileOutputStream(f))
		dos.writeInt(das.size)	// how many DocAlignments in this file
		for(da <- das) {
			val report = da.report.asInstanceOf[CommunicationDocument]
			val passage = da.passage.asInstanceOf[CommunicationDocument]
			val discourse = ConcreteDocAlignmentUtils.toDiscourse(da, report, passage)
			discourse.writeDelimitedTo(dos)
			report.communication.writeDelimitedTo(dos)
			passage.communication.writeDelimitedTo(dos)
		}
		dos.close
	}

	// DocAlignment => Discourse
	def toDiscourse(da: DocAlignment, report: CommunicationDocument, passage: CommunicationDocument): Discourse = {
		val dab = Discourse.DiscourseAnnotation.newBuilder
			.setId(IdUtil.generateUUID)
			.setMetadata(ConcreteAnnotationUtils.meta)
	
		if(verbose) log("writing discourse for " + da.id)

		// arguments/entities
		val reportEntMap: Bijection[UUID, ArgumentCoref] = report.entityMapping
		val passageEntMap: Bijection[UUID, ArgumentCoref] = passage.entityMapping
		for(aca <- da.argCorefAlignments) {
			
			val reportArgCoref = aca.reportCoref
			val passageArgCoref = aca.passageCoref

			val reportEntRef = EntityRef.newBuilder
				.setEntityId(reportEntMap.getBackwards(reportArgCoref))
				.setCommunicationId(report.communication.getUuid)

			val passageEntRef = EntityRef.newBuilder
				.setEntityId(passageEntMap.getBackwards(passageArgCoref))
				.setCommunicationId(passage.communication.getUuid)

			val conf = if(da.sureAlignments.contains(aca)) 1f else 0.5f

			dab.addDiscourseEntity(DiscourseEntity.newBuilder
				.setId(IdUtil.generateUUID)
				.setConfidence(conf)
				.addEntityRef(reportEntRef)
				.addEntityRef(passageEntRef))
		}

		// predicates/situations
		val reportSitMap: Bijection[UUID, Predicate] = report.situationMapping
		val passageSitMap: Bijection[UUID, Predicate] = passage.situationMapping
		for(pa <- da.predicateAlignments) {

			val reportPred = pa.reportPred
			val passagePred = pa.passagePred

			val reportSitRef = SituationRef.newBuilder
				.setSituationId(reportSitMap.getBackwards(reportPred))
				.setCommunicationId(report.communication.getUuid)

			val passageSitRef = SituationRef.newBuilder
				.setSituationId(passageSitMap.getBackwards(passagePred))
				.setCommunicationId(passage.communication.getUuid)

			val conf = if(da.sureAlignments.contains(pa)) 1f else 0.5f

			dab.addDiscourseSituation(DiscourseSituation.newBuilder
				.setId(IdUtil.generateUUID)
				.setConfidence(conf)
				.addSituationRef(reportSitRef)
				.addSituationRef(passageSitRef))
		}

		Discourse.newBuilder
			.setId(IdUtil.generateUUID)
			.setMetadata(ConcreteAnnotationUtils.meta)
			.addAnnotation(dab)
			.build
	}

	// Discourse => DocAlignment
	def fromDiscourse(discourse: Discourse, reportComm: Communication, passageComm: Communication): DocAlignment = {

		val report = new RichConcreteDoc(reportComm)
		val passage = new RichConcreteDoc(passageComm)

		val sureAlignments = new ArrayBuffer[Alignment]
		val possibleAlignments = new ArrayBuffer[Alignment]
		def addAlignment(a: Alignment, confidence: Double) {
			assert(0 <= confidence && confidence <= 1)
			if(confidence >= 0.75)
				sureAlignments += a
			else if(confidence >= 0.5)
				possibleAlignments += a
		}

		assert(discourse.getAnnotationList.size == 1)
		val da = discourse.getAnnotationList.head

		// argument coref alignments
		val reportEntMap: Bijection[UUID, ArgumentCoref] = report.entityMapping
		val passageEntMap: Bijection[UUID, ArgumentCoref] = passage.entityMapping
		for(discEnt <- da.getDiscourseEntityList) {
			assert(discEnt.getEntityRefList.size == 2)
			var reportArgCoref: ArgumentCoref = null
			var passageArgCoref: ArgumentCoref = null
			for(entRef <- discEnt.getEntityRefList) {
				val entUUID = entRef.getEntityId
				val commUUID = entRef.getCommunicationId
				if(commUUID == report.communication.getUuid)
					reportArgCoref = reportEntMap.getForwards(entUUID)
				else {
					assert(commUUID == passage.communication.getUuid)
					passageArgCoref = passageEntMap.getForwards(entUUID)
				}
			}
			val aca = new ArgCorefAlignment(reportArgCoref, passageArgCoref)
			addAlignment(aca, discEnt.getConfidence)
		}

		// predicate alignments
		val reportSitMap: Bijection[UUID, Predicate] = report.situationMapping
		val passageSitMap: Bijection[UUID, Predicate] = passage.situationMapping
		for(discSit <- da.getDiscourseSituationList) {
			assert(discSit.getSituationRefList.size == 2)
			var reportPred: Predicate = null
			var passagePred: Predicate = null
			for(sitRef <- discSit.getSituationRefList) {
				val sitUUID = sitRef.getSituationId
				val commUUID = sitRef.getCommunicationId
				if(commUUID == report.communication.getUuid)
					reportPred = reportSitMap.getForwards(sitUUID)
				else {
					assert(commUUID == passage.communication.getUuid)
					passagePred = passageSitMap.getForwards(sitUUID)
				}
			}
			val pa = new PredicateAlignment(reportPred, passagePred)
			addAlignment(pa, discSit.getConfidence)
		}

		val id = "r%s_p%s".format(report.id, passage.id)
		val domain = Some(report.communication.getGuid.getCorpusName)
		assert(report.communication.getGuid.getCorpusName == passage.communication.getGuid.getCorpusName)
		new DocAlignment(id, domain, report, passage, sureAlignments.toSet, possibleAlignments.toSet)
	}
}

