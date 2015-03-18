// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.features.generic

// TODO return to this

// how might you represent more than one features in one?
// val allWN = wordNetSyn + wordNetHyp + ...
// ... word >>= allWN >>= max
	
// basically i'm trying to get non-determinism out of my features
// i am defining a lattice and saying, "find all paths, make a feature for each!"
	
// i can define a type which holds N Pipes
// when in binds (to the right) with another holder with M Pipes,
// it produces another holder with N*M Pipes!

case class SuperPipe[D,T](val name: String, val pipes: Seq[Pipe[D,T]]) {
	def this(name: String, p: Pipe[D,T]) = this(name, Seq(p))
	def simpleBind[R](sp: SuperPipe[T, R]): SuperPipe[D,R] = {
		val newName = name + "-" + sp.name
		val newPipes = pipes.flatMap(p => sp.pipes.map(pp => p.bind(pp)))
		SuperPipe(newName, newPipes)
	}
}

object SuperPipeImplicits {
	implicit def p2sp[D,R](p: Pipe[D,R]): SuperPipe[D,R] = SuperPipe("super:"+p.name, Seq(p))
	implicit def |[D,R](p1: Pipe[D,R], p2: Pipe[D,R]) = SuperPipe("or", Seq(p1, p2))
}
