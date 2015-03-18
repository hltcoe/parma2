package edu.jhu.hlt.parma.util

object Color {
    implicit class AnsiColorString(val base: String) {
        def grey: String = "\033[1;30;40m" + base + "\033[m"
        def gray: String = "\033[1;30;40m" + base + "\033[m"
        def red: String = "\033[1;31;40m" + base + "\033[m"
        def green: String = "\033[1;32;40m" + base + "\033[m"
        def yellow: String = "\033[1;33;40m" + base + "\033[m"
        def blue: String = "\033[1;34;40m" + base + "\033[m"
        def purple: String = "\033[1;35;40m" + base + "\033[m"
    }
}

