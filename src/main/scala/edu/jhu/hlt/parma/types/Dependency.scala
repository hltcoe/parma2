// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.types

class Dependency[T](val typ: String, val gov: T, val dep: T) {

	def toUntyped = new Dependency[T]("untyped", gov, dep)

	def map[B](f: T => B): Dependency[B] = new Dependency(typ, f(gov), f(dep))

	def map[B](fGov: T => B, fDep: T => B) = new Dependency(typ, fGov(gov), fDep(dep))

	override def toString: String = "(Dep t=%s g=%s d=%s)".format(typ, gov, dep)

	override def equals(obj: Any): Boolean = {
		if(obj.isInstanceOf[Dependency[_]]) {
			val o = obj.asInstanceOf[Dependency[_]]
			o.typ == typ && o.gov == gov && o.dep == dep
		}
		else false
	}

	override def hashCode: Int = (typ.hashCode << 20) | (gov.hashCode << 10) | (dep.hashCode)

	def toTimeSieveStr(f: T => String): String = {
		assert(typ.split("\\s+").size == 1)
		val g = f(gov)
		val d = f(dep)
		assert(g.split("\\s+").size == 1, "gov = \"%s\"".format(g))
		assert(d.split("\\s+").size == 1, "dep = \"%s\"".format(d))
		"%s(%s %s)".format(typ, g, d)
	}
}

