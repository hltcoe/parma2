package edu.jhu.hlt.parma.util

import java.io.File

object MiscImplicits {
	implicit class RichFile(val f: File) {
		def /(name: String): File = new File(f, name)
	}
}

