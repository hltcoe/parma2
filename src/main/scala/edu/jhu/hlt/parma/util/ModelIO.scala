package edu.jhu.hlt.parma.util

import edu.jhu.hlt.parma.types._
import java.io._
import java.util.zip._

object ModelIO extends Logging2 {

	def writeModel(f: File, params: DVec, featureNames: Alphabet[String], humanReadable: Boolean = false) {
		require(featureNames.size <= params.dimension)
		if(humanReadable) {
			val w = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(f)))
			val s = featureNames.size
			val n = params.dimension
			w.write("featureNames.size %d params.size %d\n".format(s, n))
			var i = 0
			while(i < n) {
				if(i < s) {
					val fn = featureNames.lookupObject(i)
					require(!fn.contains("\t"))
					w.write("%s\t%f\n".format(fn, params(i)))
				}
				else warnIf(params(i) != 0d, "non-zero feature with no name!")
				i += 1
			}
			w.close
		}
		else {
			// use DataOutputStreams rather than text
			val dos = new DataOutputStream(new GZIPOutputStream(new FileOutputStream(f)))
			serAlph(dos, featureNames)
			serDVec(dos, params)
			dos.close
		}
	}

	def readModel(f: File, humanReadable: Boolean = false): (DVec, Alphabet[String]) = {
		if(humanReadable) {
			val r = new BufferedReader(new InputStreamReader(new FileInputStream(f)))
			val header = r.readLine
			val headerAr = header.split("\\s+")
			assert(headerAr.length == 4 && headerAr(0) == "featureNames.size" && headerAr(2) == "params.size")
			val s = headerAr(1).toInt
			val n = headerAr(2).toInt
			val v = DVec.zero(n)
			val a = new Alphabet[String]
			var i = 0
			while(i < n) {
				val ar = r.readLine.split("\t")
				assert(ar.length == 2)
				v(i) = ar(1).toDouble
				if(i < s)
					a.lookupIndex(ar(0), addIfNotPresent=true)
				else
					assert(v(i) == 0d)
				i += 1
			}
			require(!r.ready, "correct # lines?")
			r.close
			(v, a)
		}
		else {
			val dis = new DataInputStream(new GZIPInputStream(new FileInputStream(f)))
			val a = deserAlph(dis)
			val v = deserDVec(dis)
			dis.close
			(v, a)
		}
	}

	def serAlph(dos: DataOutputStream, alph: Alphabet[String]) {
		val n = alph.size
		dos.writeInt(n)
		var i = 0
		while(i < n) {
			dos.writeUTF(alph.lookupObject(i))
			i += 1
		}
	}

	def deserAlph(dis: DataInputStream): Alphabet[String] = {
		val a = new Alphabet[String]
		val n = dis.readInt
		var i = 0
		while(i < n) {
			a.lookupIndex(dis.readUTF, addIfNotPresent=true)
			i += 1
		}
		a
	}

	def serDVec(dos: DataOutputStream, vec: DVec) {
		val n = vec.dimension
		dos.writeInt(n)
		var i = 0
		while(i < n) {
			dos.writeDouble(vec(i))
			i += 1
		}
	}

	def deserDVec(dis: DataInputStream): DVec = {
		val n = dis.readInt
		val vec = DVec.zero(n)
		var i = 0
		while(i < n) {
			vec(i) = dis.readDouble
			i += 1
		}
		vec
	}
}

