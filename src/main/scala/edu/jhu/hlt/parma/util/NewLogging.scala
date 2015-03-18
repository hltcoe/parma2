package edu.jhu.hlt.parma.util

import java.io._

/**
 * log statements and warnings go to the same output stream
 */
trait Logger {
	def log(msg: String)
	def warn(msg: String)
	def getOutputStream: OutputStream
	def flush { getOutputStream.flush }
}

object StdOutLogger extends Logger {
	override def log(msg: String) { println(msg) }
	override def warn(msg: String) { println("WARNING: " + msg) }
	override def getOutputStream: OutputStream = System.out
}

object NoOpLogger extends Logger {
	override def log(msg: String) {}
	override def warn(msg: String) {}
	override def getOutputStream: OutputStream = {
		new OutputStream {
			override def write(i: Int) {}
			override def flush {}
			override def close {}
		}
	}
}

class TeeLogger(val log1: Logger, val log2: Logger) extends Logger {
	override val getOutputStream = new OutputStream {
		override def write(i: Int) {
			log1.getOutputStream.write(i)
			log2.getOutputStream.write(i)
		}
		override def flush {
			log1.getOutputStream.flush
			log2.getOutputStream.flush
		}
		override def close {
			if(!(log1.getOutputStream == System.out || log1.getOutputStream == System.err))
				log1.getOutputStream.close
			if(!(log2.getOutputStream == System.out || log2.getOutputStream == System.err))
				log2.getOutputStream.close
		}
	}
	override def log(msg: String) {
		log1.log(msg)
		log2.log(msg)
	}
	override def warn(msg: String) {
		log1.warn(msg)
		log2.warn(msg)
	}
}

object OnlyWarningsLogger extends Logger {
	override def log(msg: String) {}
	override def warn(msg: String) { println("WARNING: " + msg) }
	override def getOutputStream: OutputStream = System.out
}

class FileLogger(val file: File, val overwrite: Boolean) extends Logger {
	if(!overwrite && file.isFile)
		throw new IllegalArgumentException("can't overwrite " + file.getPath)
	if(file.isDirectory)
		throw new IllegalArgumentException("can't log to directory: " + file.getPath)
	def this(f: File) = this(f, true)
	def this(s: String) = this(new File(s))
	private val outputStream = new BufferedOutputStream(new FileOutputStream(file), 32 * 1014 * 1014)
	private val writer = new PrintWriter(new OutputStreamWriter(outputStream), true)
	override def log(msg: String) { writer.println(msg) }
	override def warn(msg: String) { writer.println("WARNING: " + msg) }
	override def getOutputStream: OutputStream = outputStream
	override def flush { writer.flush }
	def close { writer.close }
}
 
/**
 * note that redirect is of type Logging, not Logger
 * this is so that you can redirect `foo` to `bar`,
 * and if `bar` is later redirected somewhere else, `foo`'s
 * logging will be passed along.
 * the only thing to worry about is that you don't make loops.
 * i could test for this (pass along a set of Logging2's every
 * time you call log() to check for loops), but this seems
 * like overkill
 */
trait Logging2 extends Serializable {

	@transient
	private[Logging2] val defaultLogger: Logger = StdOutLogger
	@transient
	private[Logging2] var redirect: Logging2 = null

	def logIf(condition: Boolean, msg: String, flush: Boolean = false) {
		if(condition) {
			getLogger.log(msg)
			if(flush) flushLog
		}
	}

	def log(msg: String, flush: Boolean = false) {
		getLogger.log(msg)
		if(flush) flushLog
	}

	def logTo[T](l: Logger, flush: Boolean = false)(block: => T): T = {
		val temp = redirect
		redirectLogTo(l)
		val ret = block
		if(flush) flushLog
		redirect = temp
		ret
	}

	def warn(msg: String, flush: Boolean = false) {
		getLogger.warn(msg)
		if(flush) flushLog
	}

	def warnIf(condition: Boolean, msg: String, flush: Boolean = false) {
		if(condition) {
			warn(msg, flush)
			if(flush) flushLog
		}
	}

	def redirectLogTo(l: Logging2) { redirect = l }

	def redirectLogTo(l: Logger) {
		redirect = new Logging2 { override def getLogger: Logger = l }
	}

	def redirectLogTo(f: File, overwrite: Boolean = true) {
		redirectLogTo(new FileLogger(f, overwrite))
	}

	def teeLogTo(log1: Logger, log2: Logger) {
		redirectLogTo(new TeeLogger(log1, log2))
	}

	def restoreDefaultLogger { redirect = null }

	def getLogger: Logger =
		if(redirect == null) defaultLogger
		else redirect.getLogger
	
	def flushLog { getLogger.flush }
}

