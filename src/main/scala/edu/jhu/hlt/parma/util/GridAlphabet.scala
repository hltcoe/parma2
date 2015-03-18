package edu.jhu.hlt.parma.util

class GridAlphabet[T] {
    private[this] val reportIdx = new Alphabet[T]
    private[this] val passageIdx = new Alphabet[T]
    def lookupObject(rowIdx: Int, colIdx: Int): (T, T) = {
        val row = reportIdx.lookupObject(rowIdx)
        val col = passageIdx.lookupObject(colIdx)
        (row, col)
    }
    def lookupIndex(reportItem: T, passageItem: T, add: Boolean = false): (Int, Int) = {
        val row = reportIdx.lookupIndex(reportItem, addIfNotPresent=add)
        val col = passageIdx.lookupIndex(passageItem, addIfNotPresent=add)
        (row, col)
    }
    def clear {
        reportIdx.clear
        passageIdx.clear
    }
}

