package edu.jhu.hlt.parma.util

import java.io._
import java.util.zip._

/**
 * good for getting files to dump output to
 * (without having multiple processes have name collisions)
 */
class WorkingDirectory(val home: File) extends Logging2 {
	require(!home.isFile)

	val debug = false
	logIf(debug, "creating working directory at " + home.getPath)

	override def toString: String = "(WorkingDirectory %s)".format(home)

	/**
	 * zips up the contents of this directory and puts into provide file
	 * @return true if there was anything in this directory, and false
	 *         otherwise (nothing is moved in this case, putInto is untouched)
	 */
	def archiveContentsTo(putInto: File): Boolean = {
		logIf(debug, "[archiveContentsTo] home=%s putInto=%s".format(home.getPath, putInto.getPath))
		warnIf(!putInto.getName.endsWith(".zip"),
			"you should probably name this file with a .zip ending: " + putInto.getPath)
		if(home.isDirectory && !empty) {
			log("[WorkingDirectory] archiving %s => %s".format(home.getPath, putInto.getPath))
			zipUp(putInto)
			delete
			home.mkdir
			true
		}
		else false
	}

	def zipUp(putInto: File) {
		require(home.isDirectory)
		val zof = new ZipOutputStream(new FileOutputStream(putInto))
		val buf = Array.ofDim[Byte](4096)
		def helper(f: File) {
			logIf(debug, "[zipUp:helper] f=" + f.getPath)
			if(f.isFile) {
				val name = home.toPath.relativize(f.toPath).toString
				zof.putNextEntry(new ZipEntry(name))
				val fis = new FileInputStream(f)
				var len = 0
				do {
					len = fis.read(buf)
					if(len > 0)
						zof.write(buf, 0, len)
				} while(len > 0)
				zof.closeEntry
				fis.close
			}
			else f.list.foreach(fn => helper(new File(f, fn)))
		}
		helper(home)
		zof.close
	}

	def empty: Boolean = home.list.size == 0

	def delete {
		logIf(debug, "[delete] home=" + home.getPath)
		def helper(f: File) {
			if(f.isFile) f.delete
			else {
				f.list.foreach(fn => helper(new File(f, fn)))
				f.delete
			}
		}
		if(home.exists)
			helper(home)
		else
			warn("[WorkingDirectory delete] %s doesn't exist".format(home.getPath))
	}

	def /(filename: String): File = {
		if(!home.isDirectory)
			home.mkdir
		new File(home, filename)
	}

	def defaultLogFile: File = uniqFile(flags=Seq("log"), suffix = ".txt")

	def uniqFile(flags: Seq[String] = Seq(), props: Map[String, String] = Map(),
			suffix: String = ".txt", overwrite: Boolean = false): File = {
		logIf(debug, "[uniqFile] home=%s flags=%s props=%s suffix=%s".format(home.getPath, flags, props, suffix))
		if(!home.exists)
			home.mkdir
		require(home.isDirectory)
		require(flags.size + props.size > 0, "no identifying information!")
		val first =
			if(flags.size > 0) flags.mkString("-")
			else ""
		val last =
			if(props.size > 0) props.toSeq.map(kv => kv._1 + "=" + kv._2).mkString("_")
			else ""
		val mid = if(props.size > 0 && flags.size > 0) "-" else ""
		val f = new File(home, first + mid + last + suffix)
		if(f.exists) {
			if(overwrite) {
				new WorkingDirectory(f).delete
				f
			}
			else {
				val ff = new File(home, first + mid + last + "-" + System.currentTimeMillis + suffix)
				require(!ff.exists)
				ff
			}
		}
		else f
	}
}

object WorkingDirectory {
	
	/**
	 * makes a unique directory from the current time
	 */
	def unNamed(parent: File): WorkingDirectory = {
		val f = new File(parent, System.currentTimeMillis + ".wd")
		require(!f.exists)
		new WorkingDirectory(f)
	}

	/**
	 * includes partialName in the directory, but ensures you get a unique directory
	 */
	def partiallyNamed(parent: File, partialName: String): WorkingDirectory = {
		val f = new File(parent, partialName + "-" + System.currentTimeMillis + ".wd")
		require(!f.exists)
		new WorkingDirectory(f)
	}

	/**
	 * tries to use fullName as a directory, if it exists, throws exception
	 */
	def fullyNamed(parent: File, fullName: String): WorkingDirectory = {
		val f = new File(parent, fullName)
		require(!f.exists)
		new WorkingDirectory(f)
	}
}

