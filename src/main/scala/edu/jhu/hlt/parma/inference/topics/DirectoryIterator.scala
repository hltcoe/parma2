// Copyright (c) 2013, Johns Hopkins University. All rights reserved.
// This software is released under the 2-clause BSD license.
// See /LICENSE.txt

// Travis Wolfe, twolfe18@gmail.com, 30 July 2013

package edu.jhu.hlt.parma.inference.topics

import java.io._

class DirectoryIterator(f: File) extends Iterator[File] {
  private[this] val fs = Option(f.listFiles).getOrElse(Array[File]())
  private[this] var i = -1
  private[this] var recurse: DirectoryIterator = null
  def hasNext = {
    if (recurse != null && recurse.hasNext) true
    else (i+1 < fs.length)
  }
  def next = {
    if (recurse != null && recurse.hasNext) recurse.next
    else if (i+1 >= fs.length) {
      throw new java.util.NoSuchElementException("next on empty file iterator")
    }
    else {
      i += 1;
      if (fs(i).isDirectory) recurse = new DirectoryIterator(fs(i))
      fs(i)
    }
  }
}
