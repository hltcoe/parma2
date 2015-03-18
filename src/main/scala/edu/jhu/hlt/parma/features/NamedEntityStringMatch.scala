package edu.jhu.hlt.parma.features

import edu.jhu.hlt.parma.feature_interfaces._
import edu.jhu.hlt.parma.types._
import edu.jhu.coe.cale.trigger.NameTrigger
import edu.jhu.coe.cale.resolution.features.NameMatchFeatures
import edu.jhu.hlt.parma.feature_interfaces._
import java.util.Collection
import java.util.HashSet

class NameEntityStringMatch extends AlignmentSimilarity {

	override def featurize(sv: SVec, a: Alignment, report: Document, passage: Document) {

		throw new RuntimeException("re-implement me (in scala -- otherwise need to implement bool2value...)!")
/*
		// Get reportMention String, name, acronym, wordSet
		//String rMentStr = ""
		//for(String tok : AgigaDocumentUtils.getTokenizedMent(report.getAgigaDoc(), reportMention)){
		//rMentStr += tok+" "
		//}
		//rMentStr.trim(); // remove the beginning and ending whitespace
		String rMentStr = AgigaDocumentUtils.getMentionString(report.getAgigaDoc(), reportMention)

		String rMenName = NameTrigger.getPrettyName(rMentStr)[0]
		String rMenNameAcro = NameMatchFeatures.getacro(rMenName)
		rMenName = rMenName.toLowerCase()
		String[] rMenNameAr = NameTrigger.strNormalizeLC(rMenName).split("\\s+")
		HashSet<String> rNames = NameMatchFeatures.getWords(rMenNameAr)

		// Get passageMention String, name, acronym, wordSet
		//String pMentStr = ""
		//for(String tok : AgigaDocumentUtils.getTokenizedMent(passage.getAgigaDoc(), passageMention)){
		//pMentStr += tok+" "
		//}
		//pMentStr.trim()
		String pMentStr = AgigaDocumentUtils.getMentionString(passage.getAgigaDoc(), passageMention)

		String pMenName = NameTrigger.getPrettyName(pMentStr)[0]
		String pMenNameAcro = NameMatchFeatures.getacro(pMenName)
		pMenName = pMenName.toLowerCase()
		String[] pMenNameAr = NameTrigger.strNormalizeLC(pMenName).split("\\s+")
		HashSet<String> pNames = NameMatchFeatures.getWords(pMenNameAr)

		// -- exact string match
		double exactStrMatch = Double.valueOf(NameMatchFeatures.exactStringMatch(rMenName, pMenName))
		addTo.plusEquals("exactStrMatch", 0, exactStrMatch);	

		//if(exactStrMatch == BINARY_VALUE_FALSE){
		// -- rname is substring of pname
		addTo.plusEquals("subStrCheck", 0, Double.valueOf(NameMatchFeatures.isSubstring(rMenName, pMenName)))
		// -- pname is substring of rname
		addTo.plusEquals("subStrCheck", 1, Double.valueOf(NameMatchFeatures.isSubstring(pMenName, rMenName)))

		// -- the longest common substring ratio
		addTo.plusEquals("recursiveLCSRatio", 0, 
				NameMatchFeatures.recursiveLCSRatio(rMenName, pMenName))

		// -- letter order match rname - pname
		addTo.plusEquals("letterOrderMatch", 0, Double.valueOf(NameMatchFeatures.letterOrderMatch(rMenName, pMenName)))
		// -- letter order match pname - rname
		addTo.plusEquals("letterOrderMatch", 1, Double.valueOf(NameMatchFeatures.letterOrderMatch(pMenName, rMenName)))

		// -- acronym match pAcro - rname
		addTo.plusEquals("acronMatch", 0, Double.valueOf(NameMatchFeatures.exactStringMatch(pMenNameAcro, rMenName)))
		// -- acronym match rAcro - pname
		addTo.plusEquals("acronMatch", 1, Double.valueOf(NameMatchFeatures.exactStringMatch(rMenNameAcro, pMenName)))

		// Hamming distance
		//if(rMenName.length() >=5 && pMenName.length() >=5){
		// -- left hamming distance normalized by shorter name
		addTo.plusEquals("HammDist", 0, 
				NameMatchFeatures.fastHamming(rMenName, pMenName, false)/Math.min(rMenName.length(), pMenName.length()))
		// --left hamming distance normalized by longer name
		addTo.plusEquals("HammDist", 1, 
				NameMatchFeatures.fastHamming(rMenName, pMenName, true)/Math.max(rMenName.length(), pMenName.length()))

		// -- right hamming distance normalized by shorter name
		addTo.plusEquals("HammDist", 2, 
				NameMatchFeatures.fastRHamming(rMenName, pMenName, false)/Math.min(rMenName.length(), pMenName.length()))
		// --right hamming distance normalized by longer name
		addTo.plusEquals("HammDist", 3, 
				NameMatchFeatures.fastRHamming(rMenName, pMenName, true)/Math.max(rMenName.length(), pMenName.length()))
		//}

		int rLen = rNames.size()
		int pLen = pNames.size()
		rNames.retainAll(pNames)
		// -- shared name word ratio normalized by shorter rName
		addTo.plusEquals("shareWordRatio", 0, rNames.size()/rLen);	
		// -- shared name word ratio normalized by shorter pName
		addTo.plusEquals("shareWordRatio", 1, pNames.size()/pLen)


		//}
*/
	}
}
