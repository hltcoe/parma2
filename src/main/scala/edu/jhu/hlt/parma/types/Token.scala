// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.types

class ParmaToken(
	override val index: Int,
	override val getWord: String,	// TODO this is why you don't "def getX", just "def X"
	override val getLemma: String,
	override val getPosTag: String,
	override val getNerTag: String,
	override val getNormNer: String) extends Token

object RootToken extends Token {
	def index: Int = -1
    def getWord: String = "ROOT"
    def getLemma: String = "ROOT"
    def getPosTag: String = "ROOT"
    def getNerTag: String = "O"
    def getNormNer: String = "O"
}

trait Token {
	def index: Int
    def getWord: String
    def getLemma: String
    def getPosTag: String
    def getNerTag: String
    def getNormNer: String

	override def toString: String = "(Token@%d %s)".format(index, getWord)
	override def hashCode: Int = (index << 16) | getWord.hashCode
	override def equals(other: Any): Boolean = {
		if(other.isInstanceOf[Token]) {
			val o = other.asInstanceOf[Token]
			index == o.index &&
				getWord == o.getWord &&
				getLemma == o.getLemma &&
				getPosTag == o.getPosTag &&
				getNerTag == o.getNerTag &&
				getNormNer == o.getNormNer
		}
		else false
	}
}

