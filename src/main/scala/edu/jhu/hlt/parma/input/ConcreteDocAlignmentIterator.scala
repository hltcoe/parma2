package edu.jhu.hlt.parma.input

import edu.jhu.hlt.parma.types._
import edu.jhu.hlt.parma.util._
import edu.jhu.hlt.concrete.Concrete._
import collection.JavaConversions._
import java.io._

class ConcreteDocAlignmentIterator(f: File) extends Iterator[DocAlignment] {
    val dis = new DataInputStream(new FileInputStream(f))
    val n = dis.readInt // how many DocAlignments in this file
    if(n <= 0) throw new RuntimeException("[ConcreteDocAlignmentIterator] n = " + n)
    private var emitted = 0
    override def hasNext: Boolean = emitted < n
    override def next: DocAlignment = {
        val discourse = Discourse.parseDelimitedFrom(dis)
        val report = Communication.parseDelimitedFrom(dis)
        val passage = Communication.parseDelimitedFrom(dis)
        emitted += 1
        if(emitted == n)
            dis.close
        ConcreteDocAlignmentUtils.fromDiscourse(discourse, report, passage)
    }
    override def size: Int = n
    def source: File = f
}

class SafeConcreteDocAlignmentIterator(f: File) extends Iterator[DocAlignment] {
	private val unsafeIter = new ConcreteDocAlignmentIterator(f)
	private var nextUp: DocAlignment = null
	private def updatePtr {
		nextUp = null
		while(unsafeIter.hasNext && nextUp == null) {
			try {
				nextUp = unsafeIter.next
			} catch {
				case e: Exception =>
					e.printStackTrace
			}
		}
	}
	updatePtr
	override def hasNext: Boolean = nextUp != null
	override def next: DocAlignment = {
		val serve = nextUp
		updatePtr
		serve
	}
    override def size: Int = throw new UnsupportedOperationException
}

