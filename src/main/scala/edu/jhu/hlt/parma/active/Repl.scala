// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.active

// TODO i've commented this out so that i don't need to include
// the scala compiler in my jars (which adds like 15 MB
// and a few seconds to assembly/package)
//import scala.tools.nsc.Settings
//import scala.tools.nsc.interpreter.ILoop

/**
 * not customized yet, but the purpose is to interact
 * with parma via a class like
 * edu.jhu.hlt.parma.active.InteractiveLearner
class ParmaLoop extends ILoop {
	override def prompt = "$ "
	override def printWelcome {
		echo("""Welcome to the parma active learning loop!
		Try instantiating an edu.jhu.hlt.parma.active.InteractiveLearner
		and playing with it. You cad define functions in this repl,
		add them to an InteractiveLearner, and see how the affect
		performance. You can also annotate examples and find examples
		according to custom criteria.""")
		echo("""PS - this is built on scala's built-in repl, so things
		like tab completion and compile errors work""")
		echo("")
	}
}

object Repl {
	def main(args: Array[String]) {
		val settings = new Settings
		settings.usejavacp.value = true
		settings.deprecation.value = true
		new ParmaLoop().process(settings)
	}
}
 */

