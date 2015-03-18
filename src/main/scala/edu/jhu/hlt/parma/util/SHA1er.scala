// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.util
import java.security.MessageDigest

object SHA1er {

	val digester = MessageDigest.getInstance("SHA1")
	
	private def bytes2string(bytes: Array[Byte]): String =
		bytes.map(b => Integer.toString((b & 0xff) + 0x100, 16).substring(1)).mkString
	
	def digest(key: String) = bytes2string(digester.digest(key.getBytes))

}
